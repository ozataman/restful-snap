{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Snap.Restful
    (

    -- * Core API
      addResource
    , addResourceRelative
    , initRest

    -- * Splice functions
    , resourceSplices
    , itemSplices
    , resourceCSplices
    , itemCSplices
    , itemCSplice
    , unitLens
    , resourceRouter
    , resourceRoutes

    -- * Types
    , CRUD (..)
    , Resource (..)
    , DBId (..)

    -- * Generating forms and splices
    , HasFormlet (..)
    , PrimSplice (..)
    , iPrimText
    , iPrimShow
    , cPrimShow

    -- * Functions for generating paths
    , rootPath
    , indexPath
    , createPath
    , showPath
    , newPath
    , editPath
    , updatePath
    , destroyPath
    , itemActionPath
    , templatePath

    -- * Misc helpers
    , redirToItem
    , relativeRedirect
    , setFormAction
    , getFormAction

    ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char8 as Build
import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.Char                      (toUpper)
import           Data.Default
import           Data.Int
import qualified Data.Map                       as M
import           Data.Monoid
import           Data.Readable
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time
import           Data.Typeable
import           Data.Word
import           Heist
import qualified Heist.Compiled                 as C
import qualified Heist.Interpreted              as I
import           Snap.Core
import           Snap.Extras.CoreUtils
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           System.Locale
import           Text.Digestive
import           Text.Digestive.Util
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- | Enumeration of all the different types of CRUD routes.
data CRUD = RIndex
          -- ^ An item index
          | RShow
          -- ^ A single item
          | RNew
          -- ^ The form for creating a new item
          | REdit
          -- ^ The form for editing an item
          | RCreate
          -- ^ Create a new item
          | RUpdate
          -- ^ Update an item
          | RDestroy
          -- ^ Delete an item
  deriving (Eq,Show,Read,Ord)


newtype DBId = DBId { unDBId :: Word64 }
    deriving (Eq,Show,Read,Ord,Num,Typeable)


instance Default DBId where
    def = DBId 0

instance Readable DBId where fromText = return . DBId <=< fromText


------------------------------------------------------------------------------
-- | Encapsulates the data necessary to define a resource.
data Resource = Resource {
      rName              :: Text
    -- ^ A name for this resource
    , rRoot              :: Text
    -- ^ URL root for this resource
    , rResourceEndpoints :: [Text]
    -- ^ Resource level routing end points
    , rItemEndpoints     :: [Text]
    -- ^ Item/instance level routing end points
}


instance Default Resource where
    def = Resource "items" "/items" [] []


------------------------------------------------------------------------------
-- | An initializer for encapsulating RESTful resources as a standalone
-- snaplet.
initRest :: HasHeist b
         => Resource
         -> [(CRUD, Handler b () ())]
         -> [(Text, Handler b () ())]
         -> [(Text, Handler b () ())]
         -> Snaplet (Heist b)
         -> SnapletInit b ()
initRest res rHandlers rResourceActions rItemActions h =
    makeSnaplet (T.concat [rName res, "-resource"])
                (T.concat ["RESTful resource for ", rName res])
                Nothing $ addResource' resourceRoutesRelative res
                            rHandlers rResourceActions rItemActions h


------------------------------------------------------------------------------
-- | Since 'initRest' returns unit, we provide a generic unit lens here for
-- use with nestSnaplet in case you don't want to add a unit field to your
-- application state type.
unitLens :: Lens' b ()
unitLens = lens (const ()) (\a () -> a)


------------------------------------------------------------------------------
-- We need two addResource functions because we are dealing with paths in two
-- different contexts: routes and splices.  With routes, the addRoutes
-- function automatically makes things relative to the current snaplet root.
-- But that will only take effect when initRest is used, and is therefore
-- inside a nestSnaplet call.
--
-- For paths inside splices, the snaplet addRoute infrastructure is not
-- available because these splices always run in the Handler App App monad and
-- therefore can't have access to the current snaplet root.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | One-stop convenience function to enable RESTful resources in your
-- application.  Call this function from your initializer passing it all of
-- your resources and it will add the routes and splices for you.
addResource :: HasHeist b
            => Resource
            -- ^ Resource definition
            -> [(CRUD, Handler b v ())]
            -- ^ Standard CRUD handlers
            -> [(Text, Handler b v ())]
            -- ^ Additional resource level handlers
            -> [(Text, Handler b v ())]
            -- ^ Additional instance/item level handlers
            -> Snaplet (Heist b)
            -- ^ The Heist snaplet initialized in your app's 'Initializer'
            -> Initializer b v ()
addResource res rHandlers rResourceActions rItemActions h = do
    addRoutes $ [((T.encodeUtf8 $ rRoot res) -/- ":id/:action", restfulHeistServe res)]
    addResource' resourceRoutes res rHandlers rResourceActions rItemActions h


------------------------------------------------------------------------------
-- | Just like 'addResource', but makes the handlers relative to the current
-- snaplet's root.  Use this function if you're writing a snaplet.
addResourceRelative :: HasHeist b
                    => Resource
                    -- ^ Resource definition
                    -> [(CRUD, Handler b v ())]
                    -- ^ Standard CRUD handlers
                    -> [(Text, Handler b v ())]
                    -- ^ Additional resource level handlers
                    -> [(Text, Handler b v ())]
                    -- ^ Additional instance/item level handlers
                    -> Snaplet (Heist b)
                    -- ^ The Heist snaplet initialized in your app's
                    -- 'Initializer'
                    -> Initializer b v ()
addResourceRelative res rHandlers rResourceActions rItemActions h = do
    addRoutes $ [(":id/:action", restfulHeistServe res)]
    addResource' resourceRoutesRelative res rHandlers rResourceActions rItemActions h


-------------------------------------------------------------------------------
-- | Serves the routes for a resource with heist templates.
restfulHeistServe :: HasHeist b => Resource -> Handler b v ()
restfulHeistServe res = do
    x <- runMaybeT $ do
      action <- MaybeT $ getParam "action"
      lift $ render $ mkPathB [T.encodeUtf8 (rRoot res), action]
    maybe mzero return x


------------------------------------------------------------------------------
-- | Helper function that can be used with resourceRoutes or
-- resourceRoutesRelative.
addResource' :: (Resource -> r -> s -> t -> [(ByteString, Handler b v ())])
             -> Resource
             -> r
             -> s
             -> t
             -> Snaplet (Heist b)
             -> Initializer b v ()
addResource' f res rHandlers rResourceActions rItemActions h = do
    addRoutes $ f res rHandlers rResourceActions rItemActions
    addConfig h mempty { hcInterpretedSplices = resourceSplices res
                       , hcCompiledSplices = resourceCSplices res }


------------------------------------------------------------------------------
-- | See 'addResource' for an explanation of the arguments to this
-- function. The routes returned ARE prefixed with rRoot from
-- Resource.
resourceRoutes
    :: MonadSnap m
    => Resource
    -> [(CRUD, m a)]
    -> [(Text, m a)]
    -> [(Text, m a)]
    -> [(ByteString, m a)]
resourceRoutes r rHandlers rResourceActions rItemActions =
    map (first $ (T.encodeUtf8 (rRoot r) -/-))
        (resourceRoutesRelative r rHandlers rResourceActions rItemActions)


------------------------------------------------------------------------------
-- | See 'addResource' for an explanation of the arguments to this function.
-- The routes returned are not prefixed with rRoot from Resource.
resourceRoutesRelative
    :: MonadSnap m
    => Resource
    -> [(CRUD, m a)]
    -> [(Text, m a)]
    -> [(Text, m a)]
    -> [(ByteString, m a)]
resourceRoutesRelative r rHandlers rResourceActions rItemActions =
    map (mkCrudRoute r) rHandlers ++
    map (mkResourceRoute r) rResourceActions ++
    map (mkItemRoute r) rItemActions


------------------------------------------------------------------------------
-- | Generate a route handler for the routes returned by resourceRoutes.  This
-- function does add the rRoot prefix.
resourceRouter :: MonadSnap m
               => Resource
               -> [(CRUD, m a)]
               -> [(Text, m a)]
               -> [(Text, m a)]
               -> m a
resourceRouter r as bs cs = route $ resourceRoutes r as bs cs


mkPath :: [Text] -> Text
mkPath = T.intercalate "/" . filter (not . T.null)

mkPathB :: [ByteString] -> ByteString
mkPathB = B.intercalate "/" . filter (not . B.null)

------------------------------------------------------------------------------
mkItemRoute :: Resource -> (Text, t3) -> (ByteString, t3)
mkItemRoute Resource{..} (actionName, h) =
  (T.encodeUtf8 $ mkPath [":id", actionName], h)


------------------------------------------------------------------------------
mkResourceRoute :: Resource -> (Text, t3) -> (ByteString, t3)
mkResourceRoute Resource{..} (actionName, h) =
  (T.encodeUtf8 $ mkPath [actionName], h)


------------------------------------------------------------------------------
mkCrudRoute :: MonadSnap m
            => Resource -> (CRUD, m a) -> (ByteString, m a)
mkCrudRoute r@Resource{..} (crud, h) =
    case crud of
      RIndex -> ("", ifTop $ method GET h)
      RCreate -> ( "", ifTop $ method POST (setCreateAction h))
      RShow -> ( ":id", ifTop $ method GET h)
      RNew -> ( "new", ifTop $ method GET (setCreateAction h))
      REdit -> ( T.encodeUtf8 $ mkPath [":id", "edit"]
               , ifTop $ method GET (setEditAction h))
      RUpdate -> ( T.encodeUtf8 $ mkPath [":id"]
                 , ifTop $ method POST (setEditAction h))
      RDestroy -> ( T.encodeUtf8 $ mkPath [":id", "destroy"]
                  , ifTop $ method POST h)
  where
    setCreateAction h2 = setFormAction (createPath r) h2
    setEditAction h2 = do
        _id <- getParam "id"
        maybe h2 (\i -> setFormAction (updatePath r (DBId i)) h2) (fromBS =<<_id)


------------------------------------------------------------------------------
-- | Return heist template location for given crud action
templatePath :: Resource -> CRUD -> ByteString
templatePath Resource{..} crud =
    case crud of
      RIndex -> mkPathB [r, "index"]
      RCreate -> error "Create action does not get a template."
      RShow -> mkPathB [r, "show"]
      RNew -> mkPathB [r, "new"]
      REdit -> mkPathB [r, "edit"]
      RUpdate -> error "Update action does not get a template."
      RDestroy -> error "Destroy action does not get a template."
  where
    r = T.encodeUtf8 rRoot


resourceActionPath :: Resource -> Text -> Text
resourceActionPath Resource{..} t = mkPath [rRoot, t]


------------------------------------------------------------------------------
-- | Generates a path for an item action.
itemActionPath :: Resource -> Text -> DBId -> Text
itemActionPath Resource{..} t DBId{..} =
    mkPath [rRoot, showT unDBId, t]


------------------------------------------------------------------------------
-- | Generates the path for the resource index.
indexPath :: Resource -> Text
indexPath r = rRoot r


------------------------------------------------------------------------------
-- | Generates the path for creating a resource.
createPath :: Resource -> Text
createPath r = rRoot r


------------------------------------------------------------------------------
-- | Generates the path for a form to a new resource.
newPath :: Resource -> Text
newPath r = mkPath [rRoot r, "new"]


------------------------------------------------------------------------------
-- | Same as 'indexPath'.
rootPath :: Resource -> Text
rootPath = indexPath


------------------------------------------------------------------------------
-- | Generates the path for a form to a new resource.
editPath :: Resource -> DBId -> Text
editPath r (DBId _id) = mkPath [rRoot r, showT _id, "edit"]


------------------------------------------------------------------------------
-- | Generates the path for showing a single resource item.
showPath :: Resource -> DBId -> Text
showPath r (DBId _id) = mkPath [rRoot r, showT _id]


------------------------------------------------------------------------------
-- | Generates the path for updating a single resource item.
updatePath :: Resource -> DBId -> Text
updatePath r (DBId _id) = mkPath [rRoot r, showT _id]


------------------------------------------------------------------------------
-- | Generates the path for deleting a resource item.
destroyPath :: Resource -> DBId -> Text
destroyPath r (DBId _id) = mkPath [rRoot r, showT _id, "destroy"]


------------------------------------------------------------------------------
-- | Sets the @RESTFormAction@ param. 
setFormAction :: MonadSnap m => Text -> m a -> m a
setFormAction a = localRequest f
  where
    f req = req { rqParams = M.insert "RESTFormAction" [T.encodeUtf8 a]
                                      (rqParams req) }

------------------------------------------------------------------------------
-- | Gets the @RESTFormAction@ param. 
getFormAction :: MonadSnap m => HeistT n m [X.Node]
getFormAction = do
    p <- lift $ getParam "RESTFormAction"
    maybe (return []) (I.textSplice . T.decodeUtf8) p


-------------------------------------------------------------------------------
-- | Paths at the resource/collection level
resourceSplices :: Monad m => Resource -> Splices (HeistT n m Template)
resourceSplices r@Resource{..} =
    sequence_ (map (mkResourceActionSplice r) rResourceEndpoints) `mappend` a
  where
    a = do
        T.concat [rName, "NewPath"] ## I.textSplice $ newPath r
        T.concat [rName, "IndexPath"] ## I.textSplice $ indexPath r
        T.concat [rName, "CreatePath"] ## I.textSplice $ createPath r
        T.concat [rName, "Path"] ## I.textSplice $ rootPath r
   


------------------------------------------------------------------------------
-- | Generates path splices for a resource item.  These splices let you put
-- resource links in your templates in DRY manner.
itemSplices :: Monad m => Resource -> DBId -> Splices (I.Splice m)
itemSplices r@Resource{..} dbid =
    sequence_ (map (mkItemActionSplice r dbid) rItemEndpoints) `mappend` a
  where
    a = do
        T.concat [rName, "ItemEditPath"] ## I.textSplice $ editPath r dbid
        T.concat [rName, "ItemShowPath"] ## I.textSplice $ showPath r dbid
        T.concat [rName, "ItemUpdatePath"] ## I.textSplice $ updatePath r dbid
        T.concat [rName, "ItemDestroyPath"] ## I.textSplice $ destroyPath r dbid
        T.concat [rName, "ItemNewPath"] ## I.textSplice $ newPath r
        T.concat [rName, "ItemIndexPath"] ## I.textSplice $ indexPath r
        T.concat [rName, "ItemCreatePath"] ## I.textSplice $ createPath r


-------------------------------------------------------------------------------
-- | Returns compiled splices for a resource.
resourceCSplices :: MonadSnap m => Resource -> Splices (C.Splice m)
resourceCSplices r = mapS (C.runNodeList =<<) $ resourceSplices r


------------------------------------------------------------------------------
-- | Generates compiled path splices for a resource item.  These splices let
-- you put resource links in your templates in DRY manner.
itemCSplices :: Resource -> Splices (Maybe DBId -> Text)
itemCSplices r@Resource{..} = a `mappend` b `mappend` c
  where
    a = do
        T.concat [rName, "ItemEditPath"] ## maybe "" (editPath r)
        T.concat [rName, "ItemShowPath"] ## maybe "" (showPath r)
        T.concat [rName, "ItemUpdatePath"] ## maybe "" (updatePath r)
        T.concat [rName, "ItemDestroyPath"] ## maybe "" (destroyPath r)
    b = mapS const $ do
      T.concat [rName, "ItemNewPath"] ## newPath r
      T.concat [rName, "ItemIndexPath"] ## indexPath r
      T.concat [rName, "ItemCreatePath"] ## createPath r
    c = sequence_ $ map (mkItemActionCSplice r) rItemEndpoints


------------------------------------------------------------------------------
-- | A splice that runs its children with all item splices for a resource.
-- This function gets the id from the \"id\" param, which could have come in
-- the request or might have been set up by a route capture string.
itemCSplice r =
    C.withSplices C.runChildren (mapS (C.pureSplice . C.textSplice) $ itemCSplices r) $ do
        mid <- lift $ getParam "id"
        return $ fromBS =<< mid


-------------------------------------------------------------------------------
-- | Splices to generate links for resource item actions.
mkItemActionSplice :: Monad m
                   => Resource -> DBId -> Text -> Splices (I.Splice m)
mkItemActionSplice r@Resource{..} dbid t =
    T.concat [rName, "Item", cap t, "Path"] ## I.textSplice $ itemActionPath r t dbid


-------------------------------------------------------------------------------
-- | Compiled splices to generate links for resource actions.
mkResourceActionSplice :: Monad m => Resource -> Text -> Splices (HeistT n m Template)
mkResourceActionSplice r@Resource{..} t =
    T.concat [rName, cap t, "Path"] ## I.textSplice $ resourceActionPath r t


-------------------------------------------------------------------------------
-- | Compiled splices to generate links for resource item actions.
mkItemActionCSplice :: Resource -> Text -> Splices (Maybe DBId -> Text)
mkItemActionCSplice r@Resource{..} t =
  T.concat [rName, "Item", cap t, "Path"] ## maybe mempty (itemActionPath r t)


------------------------------------------------------------------------------
-- | Redirect to given item's default show page
redirToItem :: MonadSnap m => Resource -> DBId -> m a
redirToItem r dbid = redirect . T.encodeUtf8 $ showPath r dbid


------------------------------------------------------------------------------
showT :: Show a => a -> Text
showT = T.pack . show


------------------------------------------------------------------------------
cap :: Text -> Text
cap t =
  case T.uncons t of
    Just (h, rest) -> T.cons (toUpper h) rest
    Nothing -> t


relativeRedirect :: MonadSnap m => B.ByteString -> m b
relativeRedirect _path = do
    root <- withRequest (return . rqContextPath)
    redirect $ root `B.append` _path


------------------------------------------------------------------------------
-- | Type class for automatic formlet generation.
class HasFormlet a where
    formlet :: Monad m => Formlet Text m a

instance HasFormlet String where formlet = string
instance HasFormlet Text where formlet = text
instance HasFormlet Int where formlet = stringRead "must be an integer"
instance HasFormlet Integer where formlet = stringRead "must be an integer"
instance HasFormlet Float where formlet = stringRead "must be a float"
instance HasFormlet Double where formlet = stringRead "must be a double"
instance HasFormlet Bool where formlet = bool

instance HasFormlet Int8 where
    formlet = stringRead "must be an integer"
instance HasFormlet Int16 where
    formlet = stringRead "must be an integer"
instance HasFormlet Int32 where
    formlet = stringRead "must be an integer"
instance HasFormlet Int64 where
    formlet = stringRead "must be an integer"
instance HasFormlet Word8 where
    formlet = stringRead "must be a positive integer"
instance HasFormlet Word16 where
    formlet = stringRead "must be a positive integer"
instance HasFormlet Word32 where
    formlet = stringRead "must be a positive integer"
instance HasFormlet Word64 where
    formlet = stringRead "must be a positive integer"

validDate :: Text -> Result Text Day
validDate = maybe (Error "invalid date") Success .
              parseTime defaultTimeLocale "%F" . T.unpack


dayText :: Day -> Text
dayText = T.pack . formatTime defaultTimeLocale "%F"


------------------------------------------------------------------------------
--                                 Splices
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Type class for automatic splice generation.
class PrimSplice a where
    iPrimSplice :: Monad m => a -> m [X.Node]
    cPrimSplice :: a -> Builder

iPrimText :: Monad m => Text -> m [X.Node]
iPrimText t = return [X.TextNode t]
iPrimShow :: (Monad m, Show a) => a -> m [X.Node]
iPrimShow = iPrimText . T.pack . show

cPrimShow :: Show a => a -> Builder
cPrimShow x = Build.fromString $ show x

instance PrimSplice String where
    iPrimSplice x = iPrimText $ T.pack x
    cPrimSplice x = Build.fromText $ T.pack x
instance PrimSplice Text where
    iPrimSplice x = iPrimText x
    cPrimSplice x = Build.fromText x
instance PrimSplice Int where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Integer where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Float where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Double where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Bool where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow

instance PrimSplice Int8 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Int16 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Int32 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Int64 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow

instance PrimSplice Word8 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Word16 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Word32 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow
instance PrimSplice Word64 where
    iPrimSplice x = iPrimShow x
    cPrimSplice = cPrimShow

instance PrimSplice Day where
    iPrimSplice = iPrimSplice . dayText
    cPrimSplice = cPrimSplice . dayText
instance PrimSplice UTCTime where
    iPrimSplice = iPrimShow
    cPrimSplice = cPrimShow

instance PrimSplice a => PrimSplice (Maybe a) where
    iPrimSplice Nothing  = iPrimText ""
    iPrimSplice (Just x) = iPrimSplice x
    cPrimSplice Nothing  = mempty
    cPrimSplice (Just x) = cPrimSplice x


