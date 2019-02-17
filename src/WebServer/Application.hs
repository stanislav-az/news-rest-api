{-# LANGUAGE OverloadedStrings #-}

module WebServer.Application where

import qualified Network.Wai                   as W
                                                ( Request(..)
                                                , Response(..)
                                                , Application(..)
                                                , Middleware(..)
                                                , pathInfo
                                                , requestMethod
                                                , responseStatus
                                                )
import qualified Data.List                     as L
                                                ( intersperse )
import qualified Network.HTTP.Types            as HTTP
                                                ( statusCode )
import           WebServer.Router               ( Route(..)
                                                , route
                                                )
import           WebServer.HandlerClass         ( MonadLogger(..) )
import           WebServer.HandlerMonad         ( DynamicPathsMap(..) )
import           Helpers                        ( texify )

newsServer
  :: [(Route, b)]
  -> (W.Request -> DynamicPathsMap -> b -> IO W.Response)
  -> W.Application
newsServer rs runH req respond = route rs req runH >>= respond

withLogging :: W.Middleware
withLogging app req respond = app
  req
  (\res -> do
    let status = texify $ HTTP.statusCode $ W.responseStatus res
        method = texify $ W.requestMethod req
        path   = mconcat $ "/" : L.intersperse "/" (W.pathInfo req)
    logDebug $ method <> " " <> path <> " " <> status
    respond res
  )
