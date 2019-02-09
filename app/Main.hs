module Main where

import           WebServer.Application
import           Database.Migration
import qualified Config                        as C
import qualified Control.Logger.Simple         as L
import           WebServer.HandlerClass
import           Network.Wai.Handler.Warp       ( run )

main :: IO ()
main = C.getLogConfig >>= \logConf -> L.withGlobalLogging logConf $ do
  logInfo "Starting server at: http://localhost:8080/"
  run 8080 (logging app)
