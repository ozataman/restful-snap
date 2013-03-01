{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Snap.Restful
    ( CRUD (..)
    , Resource (..)
    , DBId (..)
    , HasFormlet (..)
    , PrimSplice (..)
    , iPrimText
    , iPrimShow
    , cPrimShow


    , initRest
    , resourceRouter
    , resourceRoutes

    , rootPath
--    , crudPath
    , indexPath
    , createPath
    , showPath
    , newPath
    , editPath
    , updatePath
    , destroyPath
    , itemActionPath

    , templatePath

    , resourceSplices
    , itemSplices
    , resourceCSplices
    , itemCSplices

    , redirToItem

    , prefixSplices

    , relativeRedirect

    , setFormAction
    , getFormAction

    , simpleDateFormlet
    ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char8 as Build
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Char             (toUpper)
import           Data.Default
import           Data.Int
import qualified Data.Map              as M
import           Data.Monoid
import           Data.Readable
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Time
import           Data.Typeable
import           Data.Word
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           System.Locale
import qualified Text.XmlHtml as X
import           Text.Digestive
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Adds a prefix to the tag names for a list of splices.  If the existing
-- tag name is empty, then the new tag name is just the prefix.  Otherwise the
-- new tag name is the prefix followed by an underscore followed by the
-- existing name.
prefixSplices :: Text -> [(Text, a)] -> [(Text, a)]
prefixSplices pre = map f
  where
    f (t,v) = if T.null t then (pre,v) else (T.concat [pre,"_",t], v)



data CRUD = RIndex
          -- ^ Gets an item index
          | RShow
          -- ^ Gets a single item
          | RNew
          -- ^ Get the form for creating a new item
          | REdit
          -- ^ Get the form for editing an item
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


data Resource b v a = Resource {
      rName :: Text
    -- ^ A name for this resource
    , rRoot :: Text
    -- ^ URL root for this resource
    , rHandlers :: [(CRUD, Handler b v a)]
    -- ^ Standard CRUD handlers
    , rResourceActions :: [(Text, Handler b v a)]
    -- ^ Additional resource level handlers
    , rItemActions :: [(Text, Handler b v a)]
    -- ^ Additional resource instance/item level handlers
    }

instance Default (Resource b v a) where
    def = Resource "items" "/items" [] [] []


------------------------------------------------------------------------------
-- | One-stop convenience function to enable RESTful resources in your
-- application.  Call this function from your initializer passing it all of
-- your resources and it will add the routes and splices for you.
initRest :: HasHeist b => [Resource b v ()] -> Initializer b v ()
initRest resources = do
    let splices = concatMap resourceSplices resources
        routes = concatMap resourceRoutes resources
    addSplices splices
    addRoutes routes


------------------------------------------------------------------------------
resourceRoutes :: Resource b v a -> [(ByteString, Handler b v a)]
resourceRoutes r@Resource{..} =
    map (mkCrudRoute r) rHandlers ++
    map (mkResourceRoute r) rResourceActions ++
    map (mkItemRoute r) rItemActions


------------------------------------------------------------------------------
resourceRouter :: Resource b v a -> Handler b v a
resourceRouter = route . resourceRoutes


------------------------------------------------------------------------------
mkItemRoute :: Resource t t1 t2 -> (Text, t3) -> (ByteString, t3)
mkItemRoute Resource{..} (act, h) =
  (T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id", act], h)


------------------------------------------------------------------------------
mkResourceRoute :: Resource t t1 t2 -> (Text, t3) -> (ByteString, t3)
mkResourceRoute Resource{..} (act, h) =
  (T.encodeUtf8 $ T.intercalate "/" [rRoot, act], h)


------------------------------------------------------------------------------
mkCrudRoute :: MonadSnap m
            => Resource b v a -> (CRUD, m a) -> (ByteString, m a)
mkCrudRoute r@Resource{..} (crud, h) =
    case crud of
      RIndex -> (T.encodeUtf8 rRoot, ifTop $ method GET h)
      RCreate -> ( T.encodeUtf8 rRoot
                 , ifTop $ method POST (setCreateAction h))
      RShow -> ( T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id"]
               , ifTop $ method GET h)
      RNew -> ( T.encodeUtf8 $ T.intercalate "/" [rRoot, "new"]
              , ifTop $ method GET (setCreateAction h))
      REdit -> ( T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id", "edit"]
               , ifTop $ method GET (setEditAction h))
      RUpdate -> ( T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id"]
                 , ifTop $ method POST (setEditAction h))
      RDestroy -> ( T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id", "destroy"]
                  , ifTop $ method POST h)
  where
    setCreateAction h2 = setFormAction (createPath r) h2
    setEditAction h2 = do
        _id <- getParam "id"
        maybe h2 (\i -> setFormAction (updatePath r (DBId i)) h2) (fromBS =<<_id)


------------------------------------------------------------------------------
-- | Return heist template location for given crud action
templatePath :: Resource t t1 t2 -> CRUD -> ByteString
templatePath Resource{..} crud =
    case crud of
      RIndex -> B.intercalate "/" [r, "index"]
      RCreate -> error "Create action does not get a template."
      RShow -> B.intercalate "/" [r, "show"]
      RNew -> B.intercalate "/" [r, "new"]
      REdit -> B.intercalate "/" [r, "edit"]
      RUpdate -> error "Update action does not get a template."
      RDestroy -> error "Destroy action does not get a template."
  where
    r = T.encodeUtf8 rRoot


------------------------------------------------------------------------------
--crudPath :: Resource b v a -> CRUD -> DBId -> Text
--crudPath Resource{..} crud DBId{..} =
--  case crud of
--    RIndex -> rRoot
--    RCreate -> rRoot
--    RShow -> T.intercalate "/" [rRoot, showT unDBId]
--    RNew -> T.intercalate "/" [rRoot, "new"]
--    REdit -> T.intercalate "/" [rRoot, showT unDBId, "edit"]
--    RUpdate -> T.intercalate "/" [rRoot, showT unDBId]
--    RDestroy -> T.intercalate "/" [rRoot, showT unDBId, "destroy"]
--    x -> error $ "Unimplemented crudpath " ++ show x


------------------------------------------------------------------------------
itemActionPath :: Resource t t1 t2 -> Text -> DBId -> Text
itemActionPath Resource{..} t DBId{..} =
    T.intercalate "/" [rRoot, showT unDBId, t]


------------------------------------------------------------------------------
indexPath :: Resource b v a -> Text
indexPath r = rRoot r


------------------------------------------------------------------------------
createPath :: Resource b v a -> Text
createPath r = rRoot r


------------------------------------------------------------------------------
newPath :: Resource b v a -> Text
newPath r = T.intercalate "/" [rRoot r, "new"]


------------------------------------------------------------------------------
rootPath :: Resource b v a -> Text
rootPath = indexPath


------------------------------------------------------------------------------
editPath :: Resource b v a -> DBId -> Text
editPath r (DBId _id) = T.intercalate "/" [rRoot r, showT _id, "edit"]


------------------------------------------------------------------------------
showPath :: Resource b v a -> DBId -> Text
showPath r (DBId _id) = T.intercalate "/" [rRoot r, showT _id]


------------------------------------------------------------------------------
updatePath :: Resource b v a -> DBId -> Text
updatePath r (DBId _id) = T.intercalate "/" [rRoot r, showT _id]


------------------------------------------------------------------------------
destroyPath :: Resource b v a -> DBId -> Text
destroyPath r (DBId _id) = T.intercalate "/" [rRoot r, showT _id, "destroy"]


setFormAction :: MonadSnap m => Text -> m a -> m a
setFormAction a = localRequest f
  where
    f req = req { rqParams = M.insert "RESTFormAction" [T.encodeUtf8 a]
                                      (rqParams req) }

getFormAction :: MonadSnap m => HeistT n m [X.Node]
getFormAction = do
    p <- lift $ getParam "RESTFormAction"
    maybe (return []) (I.textSplice . T.decodeUtf8) p


-------------------------------------------------------------------------------
resourceSplices :: Monad m => Resource b v a -> [(Text, HeistT n m Template)]
resourceSplices r@Resource{..} =
  [ (T.concat [rName, "NewPath"], I.textSplice $ newPath r)
  , (T.concat [rName, "IndexPath"], I.textSplice $ indexPath r)
  , (T.concat [rName, "CreatePath"], I.textSplice $ createPath r)
  , (T.concat [rName, "Path"], I.textSplice $ rootPath r)

  -- This splice is designed to be used in create and update forms to specify
  -- the correct action URL.
  --, ("RESTFormAction", undefined)
  ]

------------------------------------------------------------------------------
itemSplices :: Monad m => Resource b v a -> DBId -> [(Text, I.Splice m)]
itemSplices r@Resource{..} dbid =
  map (mkItemActionSplice r dbid . fst) rItemActions ++
  [ (T.concat [rName, "ItemEditPath"], I.textSplice $ editPath r dbid)
  , (T.concat [rName, "ItemShowPath"], I.textSplice $ showPath r dbid)
  , (T.concat [rName, "ItemUpdatePath"], I.textSplice $ updatePath r dbid)
  , (T.concat [rName, "ItemDestroyPath"], I.textSplice $ destroyPath r dbid)
  , (T.concat [rName, "ItemNewPath"], I.textSplice $ newPath r)
  , (T.concat [rName, "ItemIndexPath"], I.textSplice $ indexPath r)
  , (T.concat [rName, "ItemCreatePath"], I.textSplice $ createPath r)
  ]


-------------------------------------------------------------------------------
resourceCSplices :: MonadSnap m => Resource b v a -> [(Text, C.Splice m)]
resourceCSplices r = C.mapSnd (C.runNodeList =<<) (resourceSplices r)


------------------------------------------------------------------------------
itemCSplices :: Resource b v a
             -> [(Text, DBId -> Text)]
itemCSplices r@Resource{..} = 
    [ (T.concat [rName, "ItemEditPath"], editPath r)
    , (T.concat [rName, "ItemShowPath"], showPath r)
    , (T.concat [rName, "ItemUpdatePath"], updatePath r)
    , (T.concat [rName, "ItemDestroyPath"], destroyPath r)
    ] ++
    C.mapSnd const
      [ (T.concat [rName, "ItemNewPath"], newPath r)
      , (T.concat [rName, "ItemIndexPath"], indexPath r)
      , (T.concat [rName, "ItemCreatePath"], createPath r)
      ] ++
    map (mkItemActionCSplice r . fst) rItemActions


-------------------------------------------------------------------------------
mkItemActionSplice :: Monad m
                   => Resource b v a -> DBId -> Text -> (Text, I.Splice m)
mkItemActionSplice r@Resource{..} dbid t =
  ( T.concat [rName, "Item", cap t, "Path"]
  , I.textSplice $ itemActionPath r t dbid)


-------------------------------------------------------------------------------
mkItemActionCSplice :: Resource b v a -> Text -> (Text, DBId -> Text)
mkItemActionCSplice r@Resource{..} t =
  ( T.concat [rName, "Item", cap t, "Path"]
  , itemActionPath r t)


------------------------------------------------------------------------------
-- | Redirect to given item's default show page
redirToItem :: MonadSnap m => Resource b v a -> DBId -> m a
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
-- | A simple formlet for dates that 
simpleDateFormlet :: (Monad m)
                  => Maybe Day -> Form Text m Day
simpleDateFormlet d = validate validDate $
    text (dayText <$> d)


------------------------------------------------------------------------------
-- | 
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


------------------------------------------------------------------------------
-- | Type class for uniform creation of splices.  For primitives that don't
-- have field names the splices should be a list with one element and an empty
-- string for the tag name.
-- class HasSplices a where
--     iSplices :: (Monad m) => a -> [(Text, I.Splice m)]
-- --    cSplices :: (Monad m) => [(Text, C.Promise a -> C.Splice m)]
-- 
-- instance HasSplices String where
--     iSplices x = [("", I.textSplice $ T.pack x)]
-- instance HasSplices Text where
--     iSplices x = [("", I.textSplice x)]
-- instance HasSplices Int where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Integer where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Float where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Double where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- 
-- instance HasSplices Int8 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Int16 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Int32 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Int64 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- 
-- instance HasSplices Word8 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Word16 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Word32 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- instance HasSplices Word64 where
--     iSplices x = [("", I.textSplice $ T.pack $ show x)]
-- 
-- instance HasSplices Day where
--     iSplices = iSplices . dayText
-- 
-- instance HasSplices a => HasSplices (Maybe a) where
--     iSplices Nothing  = [("", I.textSplice "")]
--     iSplices (Just x) = iSplices x


