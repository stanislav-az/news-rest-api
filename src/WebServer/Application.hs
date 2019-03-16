{-# LANGUAGE OverloadedStrings #-}

module WebServer.Application where

import qualified Data.List as L (intersperse)
import Ext.Data.Text (textify)
import qualified Network.HTTP.Types as HTTP (statusCode)
import qualified Network.Wai as W
  ( Application(..)
  , Middleware(..)
  , Request(..)
  , Response(..)
  , pathInfo
  , requestMethod
  , responseStatus
  )
import WebServer.HandlerClass (MonadLogger(..))
import WebServer.HandlerMonad (DynamicPathsMap(..))
import WebServer.Router (Route(..), route)

newsServer ::
     [(Route, b)]
  -> (W.Request -> DynamicPathsMap -> b -> IO W.Response)
  -> W.Application
newsServer rs runH req respond = route rs req runH >>= respond

withLogging :: W.Middleware
withLogging app req respond =
  app
    req
    (\res -> do
       let status = textify $ HTTP.statusCode $ W.responseStatus res
           method = textify $ W.requestMethod req
           path = mconcat $ "/" : L.intersperse "/" (W.pathInfo req)
       logDebug $ method <> " " <> path <> " " <> status
       respond res)
