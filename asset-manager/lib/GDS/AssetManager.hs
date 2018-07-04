{-# LANGUAGE OverloadedStrings #-}

-- | The asset-manager service.
module GDS.AssetManager where

import           Data.Aeson                (Value)
import           Data.String               (fromString)
import qualified Data.Text                 as T
import           Data.UUID.Types           (UUID)
import qualified Network.HTTP.Types.Status as HTTP
import           Network.Wai               (responseLBS)
import           Servant
import           Servant.Multipart         (FileData, MultipartData, Tmp)
import qualified Servant.Multipart         as MP

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
upload multipartData = do
  _ <- requireFile "asset[file]" multipartData
  throwError err501

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
uploadWhitehall multipartData = do
  _ <- requireInput "asset[legacy_url_path]" multipartData
  _ <- requireFile "asset[file]" multipartData
  throwError err501

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
require msg = maybe (throwError err) pure
 where
  err = err422 { errBody = fromString ("{ errors: [\"" ++ msg ++ "\"] }") }
