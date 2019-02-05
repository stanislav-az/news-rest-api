module Main where

import           WebServer.Application
import           Network.Wai.Handler.Warp       ( run )
import           Database.Migration
import           Control.Logger.Simple
import           System.Directory               ( createDirectoryIfMissing )

---- TO DO ----
-- Exception management and logging (use handlers?)
-- Filtering, searching, sorting

main :: IO ()
main = withGlobalLogging logConf $ do
  createDirectoryIfMissing False "./logs"
  logDebug "I'm logging the log"
  logInfo "Helpful info!"
  logWarn "Beware of doggos"
  logError "AAAAAAAAA, we're all gonna die"
  putStrLn "Starting server at: "
  putStrLn "http://localhost:8080/"
  run 8080 (logging app)

logConf =
  LogConfig { lc_file = Just "./logs/news-server.log", lc_stderr = True }

-- lc_file :: !(Maybe FilePath) 
