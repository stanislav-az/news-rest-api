{-# LANGUAGE OverloadedStrings #-}

module WebServer.Router where

import qualified Data.Text                     as T
import qualified Data.ByteString               as BS
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           Network.Wai
import           Network.HTTP.Types
import           WebServer.HandlerClass
import           WebServer.HandlerMonad
import           WebServer.UrlParser.Pagination

data Route = PathRoute T.Text Route | DynamicRoute T.Text Route | MethodRoute BS.ByteString

checkout
  :: DynamicPathsMap
  -> Route
  -> [T.Text]
  -> BS.ByteString
  -> (Bool, DynamicPathsMap)
checkout dpMap (MethodRoute x) [] method | x == method = (True, dpMap)
                                         | otherwise   = (False, [])
checkout dpMap (MethodRoute x) [""] method | x == method = (True, dpMap)
                                           | otherwise   = (False, [])
checkout dpMap (MethodRoute x) xs method = (False, [])
checkout dpMap route           [] _      = (False, [])
checkout dpMap (PathRoute s route) (x : xs) method
  | x == s    = checkout dpMap route xs method
  | otherwise = (False, [])
checkout dpMap (DynamicRoute s route) (x : xs) method =
  checkout ((s, x) : dpMap) route xs method

route
  :: MonadHTTP m
  => [(Route, b)]
  -> Request
  -> (Request -> DynamicPathsMap -> b -> m Response)
  -> m Response
route [] req runH = notFoundResponse
route (h : hs) req runH | isCorrect = runH req dpMap handler
                        | otherwise = route hs req runH
 where
  (isCorrect, dpMap) = checkout [] currentRoute path method
  currentRoute       = fst h
  handler            = snd h
  path               = pathInfo req
  method             = requestMethod req

