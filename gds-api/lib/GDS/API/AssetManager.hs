{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module GDS.API.AssetManager where

import           Data.Aeson                (Object, Value, toJSON)
import qualified Data.HashMap.Strict       as M
import           Data.Proxy                (Proxy (..))
import           Data.String               (fromString)
import           Data.UUID.Types           (UUID)
import qualified Data.UUID.Types           as UUID
import qualified Network.HTTP.Types.Method as HTTP
import           Servant.API
import           Servant.Client            (ClientM, Response, client)
import           Servant.Multipart


-------------------------------------------------------------------------------
-- * API definition

-- | The asset-manager service: stores files and associated metadata.
type API =
  ("assets" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Value)
  :<|> ("assets" :> Capture "id" UUID :> MultipartForm Tmp (MultipartData Tmp) :> Put '[JSON] Value)
  :<|> ("whitehall_assets" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Value)
  :<|> NonUploadAPI

-- | Boilerplate.
api :: Proxy API
api = Proxy

-- | The bits of the API that don't involve uploading.  Due to a
-- limitation in servant, we can only automatically generate the API
-- client for this part.
type NonUploadAPI =
  ("assets" :> Capture "id" UUID :> Get '[JSON] Value)
  :<|> ("assets" :> Capture "id" UUID :> Delete '[JSON] Value)
  :<|> ("assets" :> Capture "id" UUID :> "restore" :> Post '[JSON] Value)
  :<|> ("media" :> Capture "id" UUID :> Capture "name" String :> Raw)
  :<|> ("whitehall_assets" :> CaptureAll "path" String :> Get '[JSON] Value)
  :<|> ("government" :> "uploads" :> CaptureAll "path" String :> Raw)
  :<|> ("healthcheck" :> Get '[JSON] Value)

-- | Boilerplate
nonUploadAPI :: Proxy NonUploadAPI
nonUploadAPI = Proxy


-------------------------------------------------------------------------------
-- * Client

-- | All handlers other than @/media@, @/government@, and
-- @/healthcheck@ return the JSON representation of the asset.

-- | Upload a new asset.
upload :: FilePath -> Object -> ClientM Value
upload = multipartHelper HTTP.methodPost "assets" . Just

-- | Upload a new whitehall asset.
uploadWhitehall :: FilePath -> String -> Object -> ClientM Value
uploadWhitehall filepath legacyUrlPath =
  multipartHelper HTTP.methodPost "whitehall_assets" (Just filepath)
    . M.insert (fromString "legacy_url_path") (toJSON legacyUrlPath)

-- | Update an asset.
update :: UUID -> Maybe FilePath -> Object -> ClientM Value
update uuid = multipartHelper HTTP.methodPut ("assets/" ++ UUID.toString uuid)

-- | Get the JSON representation of an asset.
retrieve :: UUID -> ClientM Value

-- | Delete an asset.
delete :: UUID -> ClientM Value

-- | Restore a deleted asset.
restore :: UUID -> ClientM Value

-- | Download an asset.
download :: UUID -> String -> ClientM Response

-- | Get the JSON representation of a whitehall asset.
retrieveWhitehall :: [String] -> ClientM Value

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
  -> ClientM Value
multipartHelper _ _ _ _ = error "unimplemented"
