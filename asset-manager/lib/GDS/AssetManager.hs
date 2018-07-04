{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | The asset-manager service.
module GDS.AssetManager where

import           Control.Monad.IO.Class
import           Data.Aeson                (Value)
import           Data.String               (fromString)
import qualified Data.Text                 as T
import           Data.UUID.Types           (UUID)
import qualified Database.MongoDB          as MongoDB
import qualified Network.HTTP.Types.Status as HTTP
import           Network.Wai               (responseLBS)
import           Servant
import           Servant.Multipart         (FileData, MultipartData, Tmp)
import qualified Servant.Multipart         as MP

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
upload _ multipartData = do
  _ <- requireFile "asset[file]" multipartData
  throwError err501

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
uploadWhitehall _ multipartData = do
  _ <- requireInput "asset[legacy_url_path]" multipartData
  _ <- requireFile "asset[file]" multipartData
  throwError err501

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
