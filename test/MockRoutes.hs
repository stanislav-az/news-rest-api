{-# LANGUAGE OverloadedStrings #-}

module MockRoutes where

import MockMonad (MockHandler(..))
import qualified Network.HTTP.Types as HTTP (status200)
import Routes (createRoutes, listRoutes, removeRoutes)
import WebServer.HandlerClass (MonadHTTP(..))
import WebServer.Router (Route(..))

mockRoutes :: [(Route, MockHandler)]
mockRoutes =
  (MethodRoute "GET", respond HTTP.status200 [] "Ok") :
  listRoutes ++ createRoutes ++ removeRoutes
