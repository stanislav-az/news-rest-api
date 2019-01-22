module Main where

import           WebServer.Application
import           Network.Wai.Handler.Warp       ( run )

main :: IO ()
main = do
  putStrLn "Starting server at: \n"
  putStrLn "http://localhost:8080/"
  run 8080 (logging app)
