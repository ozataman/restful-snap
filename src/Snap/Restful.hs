{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Snap.Restful
    ( CRUD (..)
    , Resource (..)
    , DBId (..)

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
    ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Char             (toUpper)
import           Data.Default
import           Data.Int
import qualified Data.Map              as M
import           Data.Readable
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Time
import           Data.Typeable
import           Data.Word
import           GHC.Generics
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           System.Locale
import           Text.Digestive
import qualified Text.XmlHtml as X
------------------------------------------------------------------------------


-- class HasFormlet a where
--     formlet :: Monad m => Formlet Text m a
-- 
-- instance HasFormlet String where formlet = string
-- instance HasFormlet Text where formlet = text
-- instance HasFormlet Int where formlet = stringRead "must be an integer"
-- instance HasFormlet Integer where formlet = stringRead "must be an integer"
-- instance HasFormlet Float where formlet = stringRead "must be a float"
-- instance HasFormlet Double where formlet = stringRead "must be a double"
-- 
-- instance HasFormlet Int8 where
--     formlet = stringRead "must be an integer"
-- instance HasFormlet Int16 where
--     formlet = stringRead "must be an integer"
-- instance HasFormlet Int32 where
--     formlet = stringRead "must be an integer"
-- instance HasFormlet Int64 where
--     formlet = stringRead "must be an integer"
-- instance HasFormlet Word8 where
--     formlet = stringRead "must be a positive integer"
-- instance HasFormlet Word16 where
--     formlet = stringRead "must be a positive integer"
-- instance HasFormlet Word32 where
--     formlet = stringRead "must be a positive integer"
-- instance HasFormlet Word64 where
--     formlet = stringRead "must be a positive integer"
-- 
-- instance HasFormlet PK32 where
--     formlet d = PK32 <$> stringRead "must be a primary key" (unPK32 <$> d)
-- instance HasFormlet PK64 where
--     formlet d = PK64 <$> stringRead "must be a primary key" (unPK64 <$> d)
-- instance HasFormlet FK32 where
--     formlet d = FK32 <$> stringRead "must be a foreign key" (unFK32 <$> d)
-- instance HasFormlet FK64 where
--     formlet d = FK64 <$> stringRead "must be a foreign key" (unFK64 <$> d)
-- 
-- validDate :: Text -> Result Text Day
-- validDate = maybe (Error "invalid date") Success .
--               parseTime defaultTimeLocale "%F" . T.unpack
-- 
-- dayText :: Day -> Text
-- dayText = T.pack . formatTime defaultTimeLocale "%F" 
-- 
-- ------------------------------------------------------------------------------
-- -- | A simple formlet for dates that 
-- simpleDateFormlet :: (Monad m)
--                   => Maybe Day -> Form Text m Day
-- simpleDateFormlet d = validate validDate $
--     text (dayText <$> d)
-- 
-- ------------------------------------------------------------------------------
-- -- | Type class for uniform creation of splices.  For primitives that don't
-- -- have field names the splices should be a list with one element and an empty
-- -- string for the tag name.
-- class HasISplices a where
--     splices :: (Monad m) => a -> [(Text, Splice m)]
-- 
-- instance HasISplices String where
--     splices x = [("", textSplice $ T.pack x)]
-- instance HasISplices Text where
--     splices x = [("", textSplice x)]
-- instance HasISplices Int where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Integer where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Float where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Double where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- 
-- instance HasISplices Int8 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Int16 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Int32 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Int64 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- 
-- instance HasISplices Word8 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Word16 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Word32 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- instance HasISplices Word64 where
--     splices x = [("", textSplice $ T.pack $ show x)]
-- 
-- instance HasISplices PK32 where
--     splices = splices . unPK32
-- instance HasISplices PK64 where
--     splices = splices . unPK64
-- instance HasISplices FK32 where
--     splices = splices . unFK32
-- instance HasISplices FK64 where
--     splices = splices . unFK64
-- 
-- instance HasISplices Day where
--     splices = splices . dayText
-- 
-- instance HasISplices a => HasISplices (Maybe a) where
--     splices Nothing  = [("", textSplice "")]
--     splices (Just x) = splices x


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
    deriving (Eq,Show,Read,Ord,Num,Generic,Typeable)


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
mkItemRoute Resource{..} (act, h) =
  (T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id", act], h)


------------------------------------------------------------------------------
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
    setCreateAction h = setFormAction (createPath r) h
    setEditAction h = do
        _id <- getParam "id"
        maybe h (\i -> setFormAction (updatePath r (DBId i)) h) (fromBS =<<_id)


------------------------------------------------------------------------------
-- | Return heist template location for given crud action
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


setFormAction a = localRequest f
  where
    f req = req { rqParams = M.insert "RESTFormAction" [T.encodeUtf8 a]
                                      (rqParams req) }

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
  , ("RESTFormAction", undefined)
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
cap t =
  case T.uncons t of
    Just (h, rest) -> T.cons (toUpper h) rest
    Nothing -> t


relativeRedirect :: MonadSnap m => B.ByteString -> m b
relativeRedirect _path = do
    root <- withRequest (return . rqContextPath)
    redirect $ root `B.append` _path


