{-# LANGUAGE OverloadedStrings #-}
module WebServer.Router where

import qualified Data.Text                     as T
import qualified Data.ByteString               as BS
import           Network.Wai
import           Network.HTTP.Types
import           WebServer.MonadHandler
import           Control.Exception              ( bracket )
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL

data Route = PathRoute T.Text Route | DynamicRoute T.Text Route | MethodRoute BS.ByteString

isCorrectRoute
  :: DynamicPathsMap
  -> Route
  -> [T.Text]
  -> BS.ByteString
  -> (Bool, DynamicPathsMap)
isCorrectRoute dpMap (MethodRoute x) [] method | x == method = (True, dpMap)
                                               | otherwise   = (False, [])
isCorrectRoute dpMap (MethodRoute x) [""] method | x == method = (True, dpMap)
                                                 | otherwise   = (False, [])
isCorrectRoute dpMap (MethodRoute x) xs method = (False, [])
isCorrectRoute dpMap route           [] _      = (False, [])
isCorrectRoute dpMap (PathRoute s route) (x : xs) method
  | x == s    = isCorrectRoute dpMap route xs method
  | otherwise = (False, [])
isCorrectRoute dpMap (DynamicRoute s route) (x : xs) method =
  isCorrectRoute ((s, x) : dpMap) route xs method

route :: [(Route, Handler)] -> Request -> IO Response
--authorizeAndHandle dpMap req $ snd h
route [] req = pure notFoundResponse
route (h : hs) req
  | isCorrect = do
    conf <- C.loadConfig
    bracket (DC.connect conf) PSQL.close
      $ \conn -> runHandler conf dpMap req conn handler
  | otherwise = route hs req
 where
  (isCorrect, dpMap) = isCorrectRoute [] currentRoute path method
  currentRoute       = fst h
  handler            = snd h
  path               = pathInfo req
  method             = requestMethod req

notFoundResponse :: Response
notFoundResponse =
  responseLBS status404 [("Content-Type", "text/html")] "Not found"
