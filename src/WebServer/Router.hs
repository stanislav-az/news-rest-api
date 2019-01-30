{-# LANGUAGE OverloadedStrings #-}
module WebServer.Router where

import           Data.Text
import           Data.ByteString
import           Network.Wai
import           Network.HTTP.Types
import           Handlers

data Route = PathRoute Text Route | DynamicRoute Text Route | MethodRoute ByteString

isCorrectRoute
  :: DynamicPathsMap -> Route -> [Text] -> ByteString -> (Bool, DynamicPathsMap)
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
route (h : hs) req | isCorrect = snd h dpMap req
                   | otherwise = route hs req
 where
  (isCorrect, dpMap) = isCorrectRoute [] currentRoute path method
  currentRoute       = fst h
  path               = pathInfo req
  method             = requestMethod req

notFoundResponse :: Response
notFoundResponse =
  responseLBS status404 [("Content-Type", "text/html")] "Not found"
