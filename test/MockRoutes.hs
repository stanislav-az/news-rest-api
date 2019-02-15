{-# LANGUAGE OverloadedStrings #-}

module MockRoutes where

import           Network.Wai
import           WebServer.HandlerMonad
import           WebServer.Router
import           MockMonad
import qualified Network.HTTP.Types            as HTTP
import           Routes
import           Handlers
import           WebServer.MonadDatabase
import           Serializer.User                ( requestToUser
                                                , userToResponse
                                                )

mockRoutes :: [(Route, MockHandler)]
mockRoutes =
  (MethodRoute "GET", pure $ responseLBS HTTP.status200 [] "Ok") : listRoutes

