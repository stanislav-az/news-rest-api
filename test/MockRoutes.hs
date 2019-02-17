{-# LANGUAGE OverloadedStrings #-}

module MockRoutes where

import qualified Network.HTTP.Types            as HTTP
                                                ( status200 )
import           Routes                         ( listRoutes
                                                , removeRoutes
                                                , createRoutes
                                                )
import           MockMonad                      ( MockHandler(..) )
import           WebServer.Router               ( Route(..) )
import           WebServer.HandlerClass         ( MonadHTTP(..) )

mockRoutes :: [(Route, MockHandler)]
mockRoutes =
  (MethodRoute "GET", respond HTTP.status200 [] "Ok")
    :  listRoutes
    ++ createRoutes
    ++ removeRoutes

