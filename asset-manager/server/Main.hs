module Main where

import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.Server                       (serve)

import qualified GDS.API.AssetManager                 as GDS
import qualified GDS.AssetManager                     as GDS

-- | Start the server on port 3037 (dev VM asset-manager port)
main :: IO ()
main = run 3037 (logStdoutDev (serve GDS.api GDS.server))
