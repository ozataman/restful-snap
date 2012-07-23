{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Restful
    ( CRUD (..)
    , Resource (..)
    , DBId (..)

    , resourceRouter
    , resourceRoutes

    , rootPath
    , crudPath
    , indexPath
    , createPath
    , showPath
    , newPath
    , editPath
    , updatePath
    , destroyPath
    , itemActionPath

    , resourceSplices
    , itemSplices

    ) where

-------------------------------------------------------------------------------
import           Control.Arrow
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Char             (toUpper)
import           Data.Default
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
-------------------------------------------------------------------------------


data CRUD = RIndex | RCreate | RShow | RNew | REdit | RUpdate | RDestroy
  deriving (Eq,Show,Read,Ord)


newtype DBId = DBId { unDBId :: Int } deriving (Eq,Show,Read,Ord)


instance Default DBId where
    def = DBId 0


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

-------------------------------------------------------------------------------
resourceRoutes :: Resource b v a -> [(ByteString, Handler b v a)]
resourceRoutes r@Resource{..} =
    map (mkCrudRoute r) rHandlers ++
    map (mkResourceRoute r) rResourceActions ++
    map (mkItemRoute r) rItemActions


-------------------------------------------------------------------------------
resourceRouter :: Resource b v a -> Handler b v a
resourceRouter = route . resourceRoutes


-------------------------------------------------------------------------------
mkItemRoute Resource{..} (act, h) =
    (T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id", act], h)


-------------------------------------------------------------------------------
mkResourceRoute Resource{..} (act, h) =
    (T.encodeUtf8 $ T.intercalate "/" [rRoot, act], h)

-------------------------------------------------------------------------------
mkCrudRoute
  :: MonadSnap m
  => Resource b v a -> (CRUD, m a) -> (ByteString, m a)
mkCrudRoute Resource{..} (crud, h) =
  case crud of
    RIndex -> (T.encodeUtf8 rRoot, ifTop $ method GET h)
    RCreate -> (T.encodeUtf8 rRoot, ifTop $ method POST h)
    RShow -> (T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id"], ifTop $ method GET h)
    RNew -> (T.encodeUtf8 $ T.intercalate "/" [rRoot, "new"], ifTop $ method GET h)
    REdit -> (T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id", "edit"], ifTop $ method GET h)
    RUpdate -> ( T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id"], ifTop $ method POST h)
    RDestroy -> ( T.encodeUtf8 $ T.intercalate "/" [rRoot, ":id", "destroy"]
                , ifTop $ method POST h)


-------------------------------------------------------------------------------
crudPath :: Resource b v a -> CRUD -> DBId -> Text
crudPath Resource{..} crud DBId{..} =
  case crud of
    RIndex -> rRoot
    RCreate -> rRoot
    RShow -> T.intercalate "/" [rRoot, showT unDBId]
    RNew -> T.intercalate "/" [rRoot, "new"]
    REdit -> T.intercalate "/" [rRoot, showT unDBId, "edit"]
    RUpdate -> T.intercalate "/" [rRoot, showT unDBId]
    RDestroy -> T.intercalate "/" [rRoot, showT unDBId, "destroy"]
    x -> error $ "Unimplemented crudpath " ++ show x


-------------------------------------------------------------------------------
itemActionPath Resource{..} t DBId{..} = T.intercalate "/" [rRoot, showT unDBId, t]


-------------------------------------------------------------------------------
editPath :: Resource b v a -> DBId -> Text
editPath r dbid = crudPath r REdit dbid


-------------------------------------------------------------------------------
showPath :: Resource b v a -> DBId -> Text
showPath r dbid = crudPath r RShow dbid


-------------------------------------------------------------------------------
updatePath :: Resource b v a -> DBId -> Text
updatePath r dbid = crudPath r RUpdate dbid


-------------------------------------------------------------------------------
destroyPath :: Resource b v a -> DBId -> Text
destroyPath r dbid = crudPath r RDestroy dbid


-------------------------------------------------------------------------------
indexPath :: Resource b v a -> DBId -> Text
indexPath r dbid = crudPath r RIndex dbid


-------------------------------------------------------------------------------
createPath :: Resource b v a -> DBId -> Text
createPath r dbid = crudPath r RCreate dbid


-------------------------------------------------------------------------------
newPath :: Resource b v a -> DBId -> Text
newPath r dbid = crudPath r RNew dbid


-------------------------------------------------------------------------------
rootPath :: Resource b v a -> DBId -> Text
rootPath = indexPath



-------------------------------------------------------------------------------
resourceSplices :: Resource b v a -> [(Text, SnapletSplice b b)]
resourceSplices r@Resource{..} =
  [ (T.concat [rName, "NewPath"], liftHeist . textSplice $ newPath r def)
  , (T.concat [rName, "IndexPath"], liftHeist . textSplice $ indexPath r def)
  , (T.concat [rName, "CreatePath"], liftHeist . textSplice $ createPath r def)
  , (T.concat [rName, "Path"], liftHeist . textSplice $ rootPath r def)
  ]


-------------------------------------------------------------------------------
itemSplices :: Monad m => Resource b v a -> DBId -> [(Text, Splice m)]
itemSplices r@Resource{..} dbid =
  map (mkItemActionSplice r dbid . fst) rItemActions ++
  [ (T.concat [rName, "ItemEditPath"], textSplice $ editPath r dbid)
  , (T.concat [rName, "ItemShowPath"], textSplice $ showPath r dbid)
  , (T.concat [rName, "ItemUpdatePath"], textSplice $ updatePath r dbid)
  , (T.concat [rName, "ItemDestroyPath"], textSplice $ destroyPath r dbid)
  , (T.concat [rName, "ItemNewPath"], textSplice $ newPath r dbid)
  , (T.concat [rName, "ItemIndexPath"], textSplice $ indexPath r dbid)
  , (T.concat [rName, "ItemCreatePath"], textSplice $ createPath r dbid)
  ]


-------------------------------------------------------------------------------
itemSplices':: Resource b v a -> DBId -> [(Text, SnapletSplice b v)]
itemSplices' r = map (second liftHeist) . itemSplices r


-------------------------------------------------------------------------------
mkItemActionSplice
  :: Monad m => Resource b v a -> DBId -> Text -> (Text, Splice m)
mkItemActionSplice r@Resource{..} dbid t =
  (T.concat [rName, "Item", cap t, "Path"], textSplice $ itemActionPath r t dbid)


-------------------------------------------------------------------------------
showT :: Show a => a -> Text
showT = T.pack . show


-------------------------------------------------------------------------------
cap t =
  case T.uncons t of
    Just (h, rest) -> T.cons (toUpper h) rest
    Nothing -> t
