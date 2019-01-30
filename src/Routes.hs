{-# LANGUAGE OverloadedStrings #-}
module Routes where

import           WebServer.Router
import           Network.Wai
import           Network.HTTP.Types
import           Handlers
import           Middlewares
import           Database.Queries.News

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "author" $ MethodRoute "POST"

updateAuthorRoute :: Route
updateAuthorRoute =
  PathRoute "api" $ PathRoute "author" $ DynamicRoute "id" $ MethodRoute "PATCH"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "user" $ MethodRoute "POST"

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
  , ( createNewsDraftRoute
    , createNewsDraftHandler
    ) -- Rename to create draft, Make publish
  , (updateNewsRoute, checkPermission (Owner isAuthorOfNews) updateNewsHandler)
  , (publishNewsRoute, checkPermission (Owner isAuthorOfNews) publishNewsHandler)  
  , ( MethodRoute "GET"
    , \_ _ -> pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]
