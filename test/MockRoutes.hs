{-# LANGUAGE OverloadedStrings #-}

module MockRoutes where

import           Network.Wai
import           WebServer.HandlerMonad
import           WebServer.Router
import           MockMonad
import qualified Network.HTTP.Types            as HTTP

mockRoutes :: [(Route, MockHandler)]
mockRoutes = [(MethodRoute "GET", pure $ responseLBS HTTP.status200 [] "Ok")]
