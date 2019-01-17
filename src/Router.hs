{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Data.ByteString
import           Data.Text
import           Network.Wai
import           Network.HTTP.Types

-- data List a = Cons a List | Nil
data Route = PathRoute Text Route | DynamicRoute Text Route | MethodRoute ByteString
type Handler = Request -> Response

-- /api/users/55/assign POST
userAssignRoute :: Route
userAssignRoute =
    PathRoute "api"
        $ PathRoute "users"
        $ DynamicRoute "55"
        $ PathRoute "assign"
        $ MethodRoute "POST"

userRejectRoute :: Route
userRejectRoute =
    PathRoute "api"
        $ PathRoute "users"
        $ DynamicRoute "55"
        $ PathRoute "reject"
        $ MethodRoute "POST"


routes :: [(Route, Handler)]
routes =
    [ ( userAssignRoute
      , const $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
      )
    , ( userRejectRoute
      , const $ responseLBS status410 [("Content-Type", "text/html")] "NotOk"
      )
    ]

-- reqToRoute :: Request -> Route
-- reqToRoute req = let path = pathInfo req in

parsePath :: Route -> [Text] -> ByteString ->  Bool
parsePath (MethodRoute x) [] method | x == method = True
                                    | otherwise   = False
parsePath (MethodRoute x) xs method = False
parsePath route           [] _      = False
parsePath (PathRoute s route) (x : xs) method
    | x == s    = parsePath route xs method
    | otherwise = False
parsePath (DynamicRoute s route) (x : xs) method = parsePath route xs method


route :: [(Route, Handler)] -> Request -> Response
route handlers req = undefined
