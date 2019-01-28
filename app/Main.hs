module Main where

import           WebServer.Application
import           Network.Wai.Handler.Warp       ( run )
import           Database.Migration

---- TO DO ----
-- Exception management and logging (use handlers?)
-- Isolate common patterns in handlers
-- Filtering, searching, sorting

main :: IO ()
main = do
  putStrLn "Starting server at: \n"
  putStrLn "http://localhost:8080/"
  run 8080 (logging app)
