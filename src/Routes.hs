{-# LANGUAGE OverloadedStrings #-}
module Routes where

import           WebServer.Router
import           Network.Wai
import           Network.HTTP.Types
import           Handlers
import           Middlewares

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "author" $ MethodRoute "POST"

updateAuthorRoute :: Route
updateAuthorRoute =
  PathRoute "api" $ PathRoute "author" $ DynamicRoute "id" $ MethodRoute "PATCH"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "user" $ MethodRoute "POST"

updateUserRoute :: Route
updateUserRoute =
  PathRoute "api" $ PathRoute "user" $ DynamicRoute "id" $ MethodRoute "PATCH"

getUsersListRoute :: Route
getUsersListRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "GET"

createTagRoute :: Route
createTagRoute = PathRoute "api" $ PathRoute "tag" $ MethodRoute "POST"

updateTagRoute :: Route
updateTagRoute =
  PathRoute "api" $ PathRoute "tag" $ DynamicRoute "id" $ MethodRoute "PATCH"

getTagsListRoute :: Route
getTagsListRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "GET"

createCategoryRoute :: Route
createCategoryRoute =
  PathRoute "api" $ PathRoute "category" $ MethodRoute "POST"

updateCategoryRoute :: Route
updateCategoryRoute =
  PathRoute "api" $ PathRoute "category" $ DynamicRoute "id" $ MethodRoute
    "PATCH"

getCategoriesListRoute :: Route
getCategoriesListRoute =
  PathRoute "api" $ PathRoute "categories" $ MethodRoute "GET"

createNewsRoute :: Route
createNewsRoute = PathRoute "api" $ PathRoute "news" $ MethodRoute "POST"

updateNewsRoute :: Route
updateNewsRoute =
  PathRoute "api" $ PathRoute "news" $ DynamicRoute "id" $ MethodRoute "PATCH"

getNewsListRoute :: Route
getNewsListRoute = PathRoute "api" $ PathRoute "news" $ MethodRoute "GET"

routes :: [(Route, Handler)]
routes =
  [ (createAuthorRoute  , checkPermission Admin createAuthorHandler)
  , (updateAuthorRoute  , checkPermission Admin updateAuthorHandler)
  , (getAuthorsListRoute, checkPermission Admin getAuthorsListHandler)
  , (createUserRoute    , createUserHandler)
  , ( updateUserRoute
    , updateUserHandler
    ) -- No such handler in requirements
  , (getUsersListRoute     , getUsersListHandler)
  , (createTagRoute        , checkPermission Admin createTagHandler)
  , (updateTagRoute        , checkPermission Admin updateTagHandler)
  , (getTagsListRoute      , getTagsListHandler)
  , (createCategoryRoute   , checkPermission Admin createCategoryHandler)
  , (updateCategoryRoute   , checkPermission Admin updateCategoryHandler)
  , (getCategoriesListRoute, getCategoriesListHandler)
  , (getNewsListRoute      , getNewsListHandler)
  , ( createNewsRoute
    , createNewsHandler
    ) -- Rename to create draft, Make publish
  , ( MethodRoute "GET"
    , \_ _ -> pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]
