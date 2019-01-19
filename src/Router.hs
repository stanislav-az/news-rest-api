{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Data.ByteString
import           Data.Text
import           Network.Wai
import           Network.HTTP.Types
import           Handlers

data Route = PathRoute Text Route | DynamicRoute Text Route | MethodRoute ByteString

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "author" $ MethodRoute "POST"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

routes :: [(Route, Handler)]
routes =
  [ (createAuthorRoute  , createAuthorHandler)
  , (getAuthorsListRoute, getAuthorsListHandler)
  , ( MethodRoute "GET"
    , const $ pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]

isCorrectRoute :: Route -> [Text] -> ByteString -> Bool
isCorrectRoute (MethodRoute x) [] method | x == method = True
                                         | otherwise   = False
isCorrectRoute (MethodRoute x) xs method = False
isCorrectRoute route           [] _      = False
isCorrectRoute (PathRoute s route) (x : xs) method
  | x == s    = isCorrectRoute route xs method
  | otherwise = False
isCorrectRoute (DynamicRoute s route) (x : xs) method =
  isCorrectRoute route xs method


route :: [(Route, Handler)] -> Request -> IO Response
route [] req =
  pure $ responseLBS status404 [("Content-Type", "text/html")] "Not found"
route (h : hs) req | isCorrectRoute currentRoute path method = snd h req
                   | otherwise                               = route hs req
 where
  currentRoute = fst h
  path         = pathInfo req
  method       = requestMethod req
