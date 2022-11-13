{-# LANGUAGE OverloadedStrings #-}

module MockRoutes where

import MockMonad (MockHandler(..))
import qualified Network.HTTP.Types as HTTP (status200)
import News.Routes (createRoutes, listRoutes, removeRoutes)
import News.WebServer.HandlerClass (MonadHTTP(..))
import News.WebServer.Router (Route(..))

mockRoutes :: [(Route, MockHandler)]
mockRoutes =
  (MethodRoute "GET", respond HTTP.status200 [] "Ok") :
  listRoutes ++ createRoutes ++ removeRoutes
