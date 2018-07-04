{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class               (MonadIO)
import qualified Database.MongoDB                     as MongoDB
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.Server                       (serve)

import qualified GDS.API.AssetManager                 as GDS
import qualified GDS.AssetManager                     as GDS

-- | Start the server with dev VM details:
--
-- * Listen port: 3037
-- * MongoDB host: localhost
-- * MongoDB port: default
-- * MongoDB database: govuk_assets_development
main :: IO ()
main = do
  pipe <- MongoDB.connect (MongoDB.host "127.0.0.1")
  let runMongo :: MonadIO m => GDS.RunMongo m a
      runMongo = MongoDB.access pipe MongoDB.master "govuk_assets_development"
  run 3037 (logStdoutDev (serve GDS.api (GDS.server runMongo)))
