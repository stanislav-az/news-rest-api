{-# LANGUAGE OverloadedStrings #-}

module WebServer.Application where

import           Network.Wai
import           WebServer.Router
import           WebServer.HandlerClass
import           WebServer.HandlerMonad
import           Helpers
import           Network.HTTP.Types
import           Data.List                      ( intersperse )

newsServer
  :: [(Route, b)]
  -> (Request -> DynamicPathsMap -> b -> IO Response)
  -> Application
newsServer rs runH req respond = route rs req runH >>= respond

withLogging :: Middleware
withLogging app req respond = app
  req
  (\res -> do
    let status = texify $ statusCode $ responseStatus res
        method = texify $ requestMethod req
        path   = mconcat $ "/" : intersperse "/" (pathInfo req)
    logDebug $ method <> " " <> path <> " " <> status
    respond res
  )
