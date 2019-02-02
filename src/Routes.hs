{-# LANGUAGE OverloadedStrings #-}
module Routes where

import           WebServer.Router
import           Network.Wai
import           Network.HTTP.Types
import           Handlers
import           Middlewares
import           Database.Queries.News
import           WebServer.MonadHandler

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "POST"

updateAuthorRoute :: Route
updateAuthorRoute =
  PathRoute "api" $ PathRoute "authors" $ DynamicRoute "id" $ MethodRoute
    "PATCH"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

getUsersListRoute :: Route
getUsersListRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "GET"

createTagRoute :: Route
createTagRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "POST"

updateTagRoute :: Route
updateTagRoute =
  PathRoute "api" $ PathRoute "tags" $ DynamicRoute "id" $ MethodRoute "PATCH"

getTagsListRoute :: Route
getTagsListRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "GET"

createCategoryRoute :: Route
createCategoryRoute =
  PathRoute "api" $ PathRoute "categories" $ MethodRoute "POST"

updateCategoryRoute :: Route
updateCategoryRoute =
  PathRoute "api" $ PathRoute "categories" $ DynamicRoute "id" $ MethodRoute
    "PATCH"

getCategoriesListRoute :: Route
getCategoriesListRoute =
  PathRoute "api" $ PathRoute "categories" $ MethodRoute "GET"

createNewsDraftRoute :: Route
createNewsDraftRoute = PathRoute "api" $ PathRoute "news" $ MethodRoute "POST"

updateNewsRoute :: Route
updateNewsRoute =
  PathRoute "api" $ PathRoute "news" $ DynamicRoute "id" $ MethodRoute "PATCH"

publishNewsRoute :: Route
publishNewsRoute =
  PathRoute "api" $ PathRoute "news" $ DynamicRoute "id" $ MethodRoute "POST"

getNewsListRoute :: Route
getNewsListRoute = PathRoute "api" $ PathRoute "news" $ MethodRoute "GET"

routes :: [(Route, Handler)]
routes =
  [ (createAuthorRoute     , checkPermission Admin createAuthorHandler)
  , (updateAuthorRoute     , checkPermission Admin updateAuthorHandler)
  , (getAuthorsListRoute   , checkPermission Admin getAuthorsListHandler)
  , (createUserRoute       , createUserHandler)
  , (getUsersListRoute     , getUsersListHandler)
  , (createTagRoute        , checkPermission Admin createTagHandler)
  , (updateTagRoute        , checkPermission Admin updateTagHandler)
  , (getTagsListRoute      , getTagsListHandler)
  , (createCategoryRoute   , checkPermission Admin createCategoryHandler)
  , (updateCategoryRoute   , checkPermission Admin updateCategoryHandler)
  , (getCategoriesListRoute, getCategoriesListHandler)
  , (getNewsListRoute      , getNewsListHandler)
  , (createNewsDraftRoute  , createNewsDraftHandler)
  , (updateNewsRoute, checkPermission (Owner isAuthorOfNews) updateNewsHandler)
  , ( publishNewsRoute
    , checkPermission (Owner isAuthorOfNews) publishNewsHandler
    )
  , ( MethodRoute "GET"
    , pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]
