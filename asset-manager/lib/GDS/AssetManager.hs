{-# LANGUAGE OverloadedStrings #-}

-- | The asset-manager service.
module GDS.AssetManager where

import           Data.Aeson                (Value)
import           Data.UUID.Types           (UUID)
import qualified Network.HTTP.Types.Status as HTTP
import           Network.Wai               (responseLBS)
import           Servant
import           Servant.Multipart

import qualified GDS.API.AssetManager      as GDS


-------------------------------------------------------------------------------
-- * API server

-- | The API server.
server :: Server GDS.API
server =
  upload
    :<|> update
    :<|> uploadWhitehall
    :<|> retrieve
    :<|> delete
    :<|> restore
    :<|> download
    :<|> retrieveWhitehall
    :<|> downloadWhitehall
    :<|> healthcheck


-------------------------------------------------------------------------------
-- * Handlers

-- | Upload a new asset.
upload :: MultipartData Tmp -> Handler Value
upload _ = throwError err501

-- | Update an asset.
update :: UUID -> MultipartData Tmp -> Handler Value
update _ _ = throwError err501

-- | Get the JSON representation of an asset.
retrieve :: UUID -> Handler Value
retrieve _ = throwError err501

-- | Delete an asset.
delete :: UUID -> Handler Value
delete _ = throwError err501

-- | Restore a deleted asset.
restore :: UUID -> Handler Value
restore _ = throwError err501

-- | Download an asset.
download :: UUID -> String -> Server Raw
download _ _ = Tagged $ \_ respond ->
  respond $ responseLBS HTTP.notImplemented501 [] "not implemented"

-- | Upload a new whitehall asset.
uploadWhitehall :: MultipartData Tmp -> Handler Value
uploadWhitehall _ = throwError err501

-- | Get the JSON representation of a whitehall asset.
retrieveWhitehall :: [String] -> Handler Value
retrieveWhitehall _ = throwError err501

-- | Download a whitehall asset.
downloadWhitehall :: [String] -> Server Raw
downloadWhitehall _ = Tagged $ \_ respond ->
  respond $ responseLBS HTTP.notImplemented501 [] "not implemented"

-- | Check the health of the application.
healthcheck :: Handler Value
healthcheck = throwError err501
