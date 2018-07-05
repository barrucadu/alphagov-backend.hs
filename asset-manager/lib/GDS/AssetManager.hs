{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | The asset-manager service.
module GDS.AssetManager where

import           Control.Monad             (unless)
import           Control.Monad.IO.Class
import           Data.Aeson                (Value)
import           Data.String               (fromString)
import qualified Data.Text                 as T
import           Data.Time.Clock           (getCurrentTime)
import           Data.UUID.Types           (UUID)
import qualified Data.UUID.Types           as UUID
import qualified Data.UUID.V4              as UUID
import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as MongoDB
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import           Network.Mime              (defaultMimeLookup)
import           Network.Wai               (responseFile, responseLBS)
import           Servant
import           Servant.Multipart         (FileData, MultipartData, Tmp)
import qualified Servant.Multipart         as MP
import           System.Directory          (copyFile, createDirectoryIfMissing)
import           System.FilePath           (FilePath, joinPath, takeDirectory)

import           GDS.API.AssetManager      (Asset (..))
import qualified GDS.API.AssetManager      as GDS


-------------------------------------------------------------------------------
-- * API server

type RunMongo m a = MongoDB.Action m a -> m a

-- | The API server.
server :: (forall m a . MonadIO m => RunMongo m a) -> Server GDS.API
server runMongo =
  upload runMongo
    :<|> update runMongo
    :<|> uploadWhitehall runMongo
    :<|> retrieve runMongo
    :<|> delete runMongo
    :<|> restore runMongo
    :<|> download runMongo
    :<|> retrieveWhitehall runMongo
    :<|> downloadWhitehall runMongo
    :<|> healthcheck


-------------------------------------------------------------------------------
-- * Handlers

-- | Upload a new asset.
upload
  :: (forall m a . MonadIO m => RunMongo m a)
  -> MultipartData Tmp
  -> Handler Asset
upload runMongo multipartData = do
  file  <- requireFile "asset[file]" multipartData
  asset <- makeAsset file Nothing multipartData
  runMongo (saveAsset file asset)
  pure asset

-- | Update an asset.
update
  :: (forall m a . MonadIO m => RunMongo m a)
  -> UUID
  -> MultipartData Tmp
  -> Handler Asset
update _ _ _ = throwError err501

-- | Get the JSON representation of an asset.
retrieve :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> Handler Asset
retrieve runMongo uuid =
  runMongo (findAssetInMongo ["uuid" =: toUUID uuid]) >>= \case
    Just asset -> pure asset
    Nothing    -> missingFile

-- | Delete an asset.
delete :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> Handler Asset
delete _ _ = throwError err501

-- | Restore a deleted asset.
restore :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> Handler Asset
restore _ _ = throwError err501

-- | Download an asset.
download
  :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> String -> Server Raw
download runMongo uuid _ = serveAssetFromDisk runMongo ["uuid" =: toUUID uuid]

-- | Upload a new whitehall asset.
uploadWhitehall
  :: (forall m a . MonadIO m => RunMongo m a)
  -> MultipartData Tmp
  -> Handler Asset
uploadWhitehall runMongo multipartData = do
  legacyUrlPath <- requireInput "asset[legacy_url_path]" multipartData
  file          <- requireFile "asset[file]" multipartData
  unless (take 1 legacyUrlPath == "/")
         (badParams "legacy url path should start with '/'")
  asset <- makeAsset file (Just legacyUrlPath) multipartData
  runMongo (saveAsset file asset)
  pure asset

-- | Get the JSON representation of a whitehall asset.
retrieveWhitehall
  :: (forall m a . MonadIO m => RunMongo m a) -> [String] -> Handler Asset
retrieveWhitehall runMongo segments =
  runMongo (findAssetInMongo ["legacy_url_path" =: joinPath ("/" : segments)])
    >>= \case
          Just asset -> pure asset
          Nothing    -> missingFile

-- | Download a whitehall asset.
downloadWhitehall
  :: (forall m a . MonadIO m => RunMongo m a) -> [String] -> Server Raw
downloadWhitehall runMongo segments = serveAssetFromDisk
  runMongo
  ["legacy_url_path" =: joinPath ("/" : "government" : "uploads" : segments)]

-- | Check the health of the application.
healthcheck :: Handler Value
healthcheck = throwError err501


-------------------------------------------------------------------------------
-- * Assets

-- | Make an asset, loading the replacement and redirect URL from the
-- given @MultipartData@.
--
-- The UUID is randomly generated.  Creation and update times are set
-- to now.
makeAsset
  :: MonadIO m => FileData tag -> Maybe FilePath -> MultipartData tag -> m Asset
makeAsset file legacyUrlPath multipartData = liftIO $ do
  uuid <- UUID.nextRandom
  now  <- getCurrentTime
  pure Asset
    { assetUUID          = uuid
    , assetFile          = T.unpack (MP.fdFileName file)
    , assetCreatedAt     = now
    , assetUpdatedAt     = now
    , assetDeletedAt     = Nothing
    , assetReplacement   = UUID.fromString
      =<< findInput "asset[replacement_id]" multipartData
    , assetRedirectUrl   = findInput "asset[redirect_url]" multipartData
    , assetLegacyUrlPath = legacyUrlPath
    }

-- | Save an asset's file to disk and data to mongo.
saveAsset :: MonadIO m => FileData Tmp -> Asset -> MongoDB.Action m ()
saveAsset file asset = do
  saveAssetToDisk file asset
  saveAssetToMongo asset

-- | Save an asset's file to disk.
saveAssetToDisk :: MonadIO m => FileData Tmp -> Asset -> m ()
saveAssetToDisk file asset = liftIO $ do
  let destination = assetFilePath asset
  let directory   = takeDirectory destination
  createDirectoryIfMissing True                directory
  copyFile                 (MP.fdPayload file) destination

-- | Look up an asset and serve its file from disk.
serveAssetFromDisk
  :: (forall m a . MonadIO m => RunMongo m a) -> MongoDB.Selector -> Server Raw
serveAssetFromDisk runMongo sel = Tagged $ \_ respond -> do
  masset <- runMongo (findAssetInMongo sel)
  respond $ case masset of
    -- todo: access control
    Just asset -> responseFile
      HTTP.ok200
      [(HTTP.hContentType, defaultMimeLookup (fromString (assetFile asset)))]
      (assetFilePath asset)
      Nothing
    Nothing -> responseLBS HTTP.notFound404
                           [(HTTP.hContentType, "application/json")]
                           "{ \"status\": \"not found\" }"

-- | Save an asset's data to mongo.
saveAssetToMongo :: MonadIO m => Asset -> MongoDB.Action m ()
saveAssetToMongo asset = MongoDB.insert_
  mongoCollection
  [ "uuid" =: toUUID (assetUUID asset)
  , "file" =: assetFile asset
  , "created_at" =: assetCreatedAt asset
  , "updated_at" =: assetUpdatedAt asset
  , "deleted_at" =: assetDeletedAt asset
    -- this is a difference: asset-manager uses _id, but to keep
    -- things simple I'm only using one notion of identifier: the
    -- uuid.
  , "replacement_uuid" =: toUUID <$> assetReplacement asset
  , "redirect_url" =: assetRedirectUrl asset
  , "legacy_url_path" =: assetLegacyUrlPath asset
  ]

-- | Find an asset by selector.
findAssetInMongo
  :: MonadIO m => MongoDB.Selector -> MongoDB.Action m (Maybe Asset)
findAssetInMongo sel = do
  doc <- MongoDB.findOne (MongoDB.select sel mongoCollection)
  pure (toAsset =<< doc)
 where
  toAsset doc =
    Asset
      <$> (fromUUID =<< MongoDB.lookup "uuid" doc)
      <*> (T.unpack <$> MongoDB.lookup "file" doc)
      <*> MongoDB.lookup "created_at" doc
      <*> MongoDB.lookup "updated_at" doc
      <*> pure (MongoDB.lookup "deleted_at" doc)
      <*> pure (fromUUID =<< MongoDB.lookup "replacement_uuid" doc)
      <*> pure (MongoDB.lookup "redirect_url" doc)
      <*> pure (MongoDB.lookup "legacy_url_path" doc)

-- | Get the path of an asset on disk.
assetFilePath :: Asset -> FilePath
assetFilePath asset =
  joinPath [uploadsBase, UUID.toString (assetUUID asset), assetFile asset]


-------------------------------------------------------------------------------
-- * MultipartData helpers

-- | Finds the input and throws a 422 exception if it's not there.
requireInput :: String -> MultipartData tag -> Handler String
requireInput input = require ("expected a " ++ input) . findInput input

-- | Finds the input.
findInput :: String -> MultipartData tag -> Maybe String
findInput input = fmap T.unpack . MP.lookupInput (fromString input)

-- | Finds the file and throws a 422 exception if it's not there.
requireFile :: String -> MultipartData tag -> Handler (FileData tag)
requireFile input = require ("expected a " ++ input) . findFile input

-- | Finds the file.
findFile :: String -> MultipartData tag -> Maybe (FileData tag)
findFile = MP.lookupFile . fromString


-------------------------------------------------------------------------------
-- * Utils

-- | Throw a 422 if a 'Maybe' value isn't present.
require :: String -> Maybe a -> Handler a
require msg = maybe (badParams msg) pure

-- | Throw a 404.
missingFile :: Handler a
missingFile =
  throwError err404 { errBody = fromString ("{ \"status\": \"not found\" }") }

-- | Throw a 422 with the given error.
badParams :: String -> Handler a
badParams msg = throwError err422
  { errBody = fromString ("{ \"errors\": [\"" ++ msg ++ "\"] }")
  }

-- | Base directory for uploads.
uploadsBase :: FilePath
uploadsBase = "/tmp/asset-manager-uploads"

-- | MongoDB collection name.
mongoCollection :: MongoDB.Collection
mongoCollection = "assets"

-- holy type conversion, batman!  maybe I should be using the MongoDB
-- UUID type, rather than the uuid-types UUID type.
toUUID :: UUID -> MongoDB.UUID
toUUID = MongoDB.UUID . UUID.toASCIIBytes

fromUUID :: MongoDB.UUID -> Maybe UUID
fromUUID (MongoDB.UUID uuid) = UUID.fromASCIIBytes uuid
