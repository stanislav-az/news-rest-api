{-# LANGUAGE OverloadedStrings #-}
module WebServer.Application where

import           Network.Wai
import WebServer.Router
import           Routes
import           Network.HTTP.Types
import           Data.List                      ( intersperse )

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
