module Main where

import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.Server                       (serve)

import qualified GDS.API.Example                      as GDS
import qualified GDS.Example                          as GDS

-- | Start the server on port 3000
main :: IO ()
main = do
  store <- GDS.newStore
  run 3000 (logStdoutDev (serve GDS.api (GDS.server store)))
