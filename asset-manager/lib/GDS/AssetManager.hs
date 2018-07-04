{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | The asset-manager service.
module GDS.AssetManager where

import           Control.Monad             (unless)
import           Control.Monad.IO.Class
import           Data.Aeson                (ToJSON, Value, toJSON)
import qualified Data.Aeson                as A
import           Data.String               (fromString)
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime, getCurrentTime)
import           Data.UUID.Types           (UUID)
import qualified Data.UUID.Types           as UUID
import qualified Data.UUID.V4              as UUID
import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as MongoDB
import           GHC.Generics              (Generic)
import qualified Network.HTTP.Types.Status as HTTP
import           Network.Wai               (responseLBS)
import           Servant
import           Servant.Multipart         (FileData, MultipartData, Tmp)
import qualified Servant.Multipart         as MP
import           System.Directory          (copyFile, createDirectoryIfMissing)
import           System.FilePath           (FilePath, (</>))

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
  -> Handler Value
upload runMongo multipartData = do
  file  <- requireFile "asset[file]" multipartData
  asset <- makeAsset file Nothing multipartData
  runMongo (saveAsset file asset)
  pure (toJSON asset)

-- | Update an asset.
update
  :: (forall m a . MonadIO m => RunMongo m a)
  -> UUID
  -> MultipartData Tmp
  -> Handler Value
update _ _ _ = throwError err501

-- | Get the JSON representation of an asset.
retrieve :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> Handler Value
retrieve _ _ = throwError err501

-- | Delete an asset.
delete :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> Handler Value
delete _ _ = throwError err501

-- | Restore a deleted asset.
restore :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> Handler Value
restore _ _ = throwError err501

-- | Download an asset.
download
  :: (forall m a . MonadIO m => RunMongo m a) -> UUID -> String -> Server Raw
download _ _ _ = Tagged $ \_ respond ->
  respond $ responseLBS HTTP.notImplemented501 [] "not implemented"

-- | Upload a new whitehall asset.
uploadWhitehall
  :: (forall m a . MonadIO m => RunMongo m a)
  -> MultipartData Tmp
  -> Handler Value
uploadWhitehall runMongo multipartData = do
  legacyUrlPath <- requireInput "asset[legacy_url_path]" multipartData
  file          <- requireFile "asset[file]" multipartData
  unless (take 1 legacyUrlPath == "/")
         (badParams "legacy url path should start with '/'")
  asset <- makeAsset file (Just legacyUrlPath) multipartData
  runMongo (saveAsset file asset)
  pure (toJSON asset)

-- | Get the JSON representation of a whitehall asset.
retrieveWhitehall
  :: (forall m a . MonadIO m => RunMongo m a) -> [String] -> Handler Value
retrieveWhitehall _ _ = throwError err501

-- | Download a whitehall asset.
downloadWhitehall
  :: (forall m a . MonadIO m => RunMongo m a) -> [String] -> Server Raw
downloadWhitehall _ _ = Tagged $ \_ respond ->
  respond $ responseLBS HTTP.notImplemented501 [] "not implemented"

-- | Check the health of the application.
healthcheck :: Handler Value
healthcheck = throwError err501


-------------------------------------------------------------------------------
-- * Assets

-- | An asset: either normal or whitehall.
--
-- Doesn't currently have: state, filename history, draft, etag, last
-- modified (what's the difference between that and updated?), md5
-- digest, size, access limited, parent document url.
--
-- Whitehall assets don't currently have: legacy etag, legacy last
-- modified.
data Asset = Asset
  { assetUUID          :: UUID
  , assetFile          :: FilePath
  -- ^ Just the name, not the full path.
  , assetCreatedAt     :: UTCTime
  , assetUpdatedAt     :: UTCTime
  -- ^ On creation, this is the created time.
  , assetDeletedAt     :: Maybe UTCTime
  , assetReplacement   :: Maybe UUID
  , assetRedirectUrl   :: Maybe String
  , assetLegacyUrlPath :: Maybe String
  -- ^ If this is present, it's a whitehall asset.
  } deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON Asset where
  toJSON     = A.genericToJSON assetJsonOptions
  toEncoding = A.genericToEncoding assetJsonOptions

-- | Strip the \"asset\" prefix and turn CamelCase into snake_case.
assetJsonOptions :: A.Options
assetJsonOptions = A.defaultOptions
  { A.fieldLabelModifier = A.camelTo2 '_' . drop (length ("asset" :: String))
  }

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
  let directory   = uploadsBase </> UUID.toString (assetUUID asset)
  let destination = directory </> T.unpack (MP.fdFileName file)
  createDirectoryIfMissing True                directory
  copyFile                 (MP.fdPayload file) destination

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
  where
    -- holy type conversion, batman!  maybe I should be using the
    -- MongoDB UUID type, rather than the uuid-types UUID type.
        toUUID = MongoDB.UUID . UUID.toASCIIBytes


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

-- | Throw a 422 with the given error.
badParams :: String -> Handler a
badParams msg =
  throwError err422 { errBody = fromString ("{ \"errors\": [\"" ++ msg ++ "\"] }") }

-- | Base directory for uploads.
uploadsBase :: FilePath
uploadsBase = "/tmp/asset-manager-uploads"

-- | MongoDB collection name.
mongoCollection :: MongoDB.Collection
mongoCollection = "assets"
