{-# LANGUAGE OverloadedStrings #-}

module WebServer.Router where

import qualified Data.Text                     as T
import qualified Data.ByteString               as BS
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           Network.Wai
import           Network.HTTP.Types
import           WebServer.HandlerMonad
import           Control.Exception              ( bracket )

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

route :: [(Route, Handler)] -> Request -> IO Response
route [] req = pure notFoundResponse
route (h : hs) req
  | isCorrect = do
    conf <- C.loadConfig
    bracket (DC.connect conf) PSQL.close
      $ \conn -> runHandler conf dpMap req conn handler
  | otherwise = route hs req
 where
  (isCorrect, dpMap) = checkout [] currentRoute path method
  currentRoute       = fst h
  handler            = snd h
  path               = pathInfo req
  method             = requestMethod req

notFoundResponse :: Response
notFoundResponse =
  responseLBS status404 [("Content-Type", "text/html")] "Not found"
