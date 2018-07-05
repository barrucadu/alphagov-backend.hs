{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module GDS.API.AssetManager where

import           Data.Aeson                (FromJSON, Object, ToJSON, Value)
import qualified Data.Aeson                as A
import qualified Data.HashMap.Strict       as M
import           Data.Maybe                (isJust)
import           Data.Proxy                (Proxy (..))
import           Data.Semigroup            ((<>))
import           Data.String               (fromString)
import           Data.Time.Clock           (UTCTime)
import           Data.UUID.Types           (UUID)
import qualified Data.UUID.Types           as UUID
import           GHC.Generics              (Generic)
import qualified Network.HTTP.Types.Method as HTTP
import           Servant.API
import           Servant.Client            (ClientM, Response, client)
import           Servant.Multipart


-------------------------------------------------------------------------------
-- * API definition

-- | The asset-manager service: stores files and associated metadata.
type API =
  ("assets" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Asset)
  :<|> ("assets" :> Capture "id" UUID :> MultipartForm Tmp (MultipartData Tmp) :> Put '[JSON] Asset)
  :<|> ("whitehall_assets" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Asset)
  :<|> NonUploadAPI

-- | Boilerplate.
api :: Proxy API
api = Proxy

-- | The bits of the API that don't involve uploading.  Due to a
-- limitation in servant, we can only automatically generate the API
-- client for this part.
type NonUploadAPI =
  ("assets" :> Capture "id" UUID :> Get '[JSON] Asset)
  :<|> ("assets" :> Capture "id" UUID :> Delete '[JSON] Asset)
  :<|> ("assets" :> Capture "id" UUID :> "restore" :> Post '[JSON] Asset)
  :<|> ("media" :> Capture "id" UUID :> Capture "name" String :> Raw)
  :<|> ("whitehall_assets" :> CaptureAll "path" String :> Get '[JSON] Asset)
  :<|> ("government" :> "uploads" :> CaptureAll "path" String :> Raw)
  :<|> ("healthcheck" :> Get '[JSON] Value)

-- | Boilerplate
nonUploadAPI :: Proxy NonUploadAPI
nonUploadAPI = Proxy


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

instance FromJSON Asset where
  parseJSON = A.genericParseJSON assetJsonOptions

instance ToJSON Asset where
  toJSON asset     =
    -- needs some extra fields, i'm actually not sure if the generic
    -- json serialisation is useful here or if I should just do it
    -- myself... but it does get us free deserialisation.
    let url = "/assets/" ++ UUID.toString (assetUUID asset)
        isDeleted = isJust (assetDeletedAt asset)
        A.Object obj = A.genericToJSON assetJsonOptions asset
    in A.Object (M.fromList [("id", A.toJSON url), ("deleted", A.toJSON isDeleted)] <> obj)

-- | Strip the \"asset\" prefix and turn CamelCase into snake_case.
assetJsonOptions :: A.Options
assetJsonOptions = A.defaultOptions
  { A.fieldLabelModifier = A.camelTo2 '_' . drop (length ("asset" :: String))
  }


-------------------------------------------------------------------------------
-- * Client

-- | All handlers other than @/media@, @/government@, and
-- @/healthcheck@ return the JSON representation of the asset.

-- | Upload a new asset.
upload :: FilePath -> Object -> ClientM Asset
upload = multipartHelper HTTP.methodPost "assets" . Just

-- | Upload a new whitehall asset.
uploadWhitehall :: FilePath -> String -> Object -> ClientM Asset
uploadWhitehall filepath legacyUrlPath =
  multipartHelper HTTP.methodPost "whitehall_assets" (Just filepath)
    . M.insert (fromString "legacy_url_path") (A.toJSON legacyUrlPath)

-- | Update an asset.
update :: UUID -> Maybe FilePath -> Object -> ClientM Asset
update uuid = multipartHelper HTTP.methodPut ("assets/" ++ UUID.toString uuid)

-- | Get the JSON representation of an asset.
retrieve :: UUID -> ClientM Asset

-- | Delete an asset.
delete :: UUID -> ClientM Asset

-- | Restore a deleted asset.
restore :: UUID -> ClientM Asset

-- | Download an asset.
download :: UUID -> String -> ClientM Response

-- | Get the JSON representation of a whitehall asset.
retrieveWhitehall :: [String] -> ClientM Asset

-- | Download a whitehall asset.
downloadWhitehall :: [String] -> ClientM Response

-- | Check the health of the application.
healthcheck :: ClientM Value

(retrieve, delete, restore, download, retrieveWhitehall, downloadWhitehall, healthcheck)
  = let
      retrieve' :<|> delete' :<|> restore' :<|> download' :<|> retrieveWhitehall' :<|> downloadWhitehall' :<|> healthcheck'
        = client nonUploadAPI
    in  ( retrieve'
        , delete'
        , restore'
        , \uuid filename -> download' uuid filename HTTP.methodGet
        , retrieveWhitehall'
        , \segments -> downloadWhitehall' segments HTTP.methodGet
        , healthcheck'
        )

-- | Helper for 'upload', 'uploadWhitehall', and 'update'.
multipartHelper
  :: HTTP.Method -- ^ Method
  -> String -- ^ Base path
  -> Maybe FilePath -- ^ File to upload
  -> Object -- ^ Attributes to send
  -> ClientM Asset
multipartHelper _ _ _ _ = error "unimplemented"
