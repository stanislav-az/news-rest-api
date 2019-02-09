{-# LANGUAGE OverloadedStrings #-}

module WebServer.Application where

import           Network.Wai
import           WebServer.Router
import           WebServer.HandlerClass
import           Helpers
import           Routes
import           Network.HTTP.Types
import           Data.List                      ( intersperse )

app :: Application
app req respond = route routes req >>= respond

logging :: Middleware
logging app req respond = app
  req
  (\res -> do
    let status = texify $ statusCode $ responseStatus res
        method = texify $ requestMethod req
        path   = mconcat $ "/" : intersperse "/" (pathInfo req)
    logDebug $ method <> " " <> path <> " " <> status
    respond res
  )
