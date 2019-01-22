{-# LANGUAGE OverloadedStrings #-}
module Routes where

import           WebServer.Router
import           Network.Wai
import           Network.HTTP.Types
import           Handlers

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "author" $ MethodRoute "POST"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "user" $ MethodRoute "POST"

updateUserRoute :: Route
updateUserRoute =
  PathRoute "api" $ PathRoute "user" $ DynamicRoute "id" $ MethodRoute "PATCH"

getUsersListRoute :: Route
getUsersListRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "GET"

routes :: [(Route, Handler)]
routes =
  [ (createAuthorRoute  , createAuthorHandler)
  , (getAuthorsListRoute, getAuthorsListHandler)
  , (createUserRoute    , createUserHandler)
  , (updateUserRoute    , updateUserHandler)
  , (getUsersListRoute  , getUsersListHandler)
  , ( MethodRoute "GET"
    , const $ pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]
