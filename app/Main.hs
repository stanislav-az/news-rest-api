{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Database
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp       ( run )
import           Data.List
import qualified Data.Text                     as T
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import           Serializer
import           Router

app :: Application
app req respond = route routes req >>= respond

logging :: Middleware
logging app req respond = app
  req
  (\res -> do
    let status = show $ statusCode $ responseStatus res
        method = show $ requestMethod req
        path   = show $ mconcat $ "/" : intersperse "/" (pathInfo req)
    putStrLn $ method ++ " " ++ path ++ " " ++ status
    respond res
  )

main :: IO ()
main = do
  putStrLn "Starting server at: \n"
  putStrLn "http://localhost:8080/"
  run 8080 (logging app)

