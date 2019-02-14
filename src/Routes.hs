{-# LANGUAGE OverloadedStrings #-}
module Routes where

import           WebServer.Router
import           Network.Wai
import           Network.HTTP.Types
import           Handlers
import           Middlewares
import           Database.Queries.News
import           Database.Queries.Commentary
import           WebServer.HandlerMonad
import           Database.Models.Author
import           Database.Models.News
import           Database.Models.Category
import           Database.Models.User
import           Database.Models.Tag
import           Database.Models.Commentary
import           Data.Proxy
import           WebServer.MonadDatabase
import           Serializer.User                ( requestToUser
                                                , userToResponse
                                                )
import           Serializer.Author              ( requestToAuthor
                                                , authorToResponse
                                                )
import           Serializer.Tag                 ( requestToTag
                                                , tagToResponse
                                                )
import           Serializer.Category            ( requestToCategory
                                                , categoryNestedToResponse
                                                )
import           Serializer.News                ( requestToNews
                                                , newsToResponse
                                                )

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

deleteAuthorRoute :: Route
deleteAuthorRoute =
  PathRoute "api" $ PathRoute "authors" $ DynamicRoute "id" $ MethodRoute
    "DELETE"

deleteTagRoute :: Route
deleteTagRoute =
  PathRoute "api" $ PathRoute "tags" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteNewsRoute :: Route
deleteNewsRoute =
  PathRoute "api" $ PathRoute "posts" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteCategoryRoute :: Route
deleteCategoryRoute =
  PathRoute "api" $ PathRoute "categories" $ DynamicRoute "id" $ MethodRoute
    "DELETE"

deleteUserRoute :: Route
deleteUserRoute =
  PathRoute "api" $ PathRoute "users" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteCommentRoute :: Route
deleteCommentRoute =
  PathRoute "api" $ PathRoute "comments" $ DynamicRoute "id" $ MethodRoute
    "DELETE"

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
createNewsDraftRoute = PathRoute "api" $ PathRoute "posts" $ MethodRoute "POST"

updateNewsRoute :: Route
updateNewsRoute =
  PathRoute "api" $ PathRoute "posts" $ DynamicRoute "id" $ MethodRoute "PATCH"

publishNewsRoute :: Route
publishNewsRoute =
  PathRoute "api" $ PathRoute "posts" $ DynamicRoute "id" $ MethodRoute "POST"

getNewsListRoute :: Route
getNewsListRoute = PathRoute "api" $ PathRoute "posts" $ MethodRoute "GET"

searchNewsRoute :: Route
searchNewsRoute =
  PathRoute "api"
    $ PathRoute "posts"
    $ PathRoute "search"
    $ DynamicRoute "search"
    $ MethodRoute "GET"

createCommentaryRoute :: Route
createCommentaryRoute =
  PathRoute "api"
    $ PathRoute "posts"
    $ DynamicRoute "id"
    $ PathRoute "comments"
    $ MethodRoute "POST"

getCommentariesListRoute :: Route
getCommentariesListRoute =
  PathRoute "api"
    $ PathRoute "posts"
    $ DynamicRoute "id"
    $ PathRoute "comments"
    $ MethodRoute "GET"

routes :: [(Route, Handler)]
routes =
  [ (updateAuthorRoute, checkPermission Admin updateAuthorHandler)
  , (updateTagRoute     , checkPermission Admin updateTagHandler)
  , (updateCategoryRoute, checkPermission Admin updateCategoryHandler)
  , (getNewsListRoute   , listNews)
  , (searchNewsRoute    , searchNews)
  , (updateNewsRoute, checkPermission (Owner isAuthorOfNews) updateNewsHandler)
  , ( publishNewsRoute
    , checkPermission (Owner isAuthorOfNews) publishNewsHandler
    )
  , (getCommentariesListRoute, listCommentariesHandler)
  , (MethodRoute "GET"       , okResponseWithJSONBody "Ok")
  ] ++ listRoutes ++ createRoutes ++ removeRoutes

listRoutes :: [(Route, Handler)]
listRoutes =
  [
  -- , (getAuthorsListRoute   , checkPermission Admin $ list authorToResponse)
   (getUsersListRoute, list selectUsers userToResponse)
  -- , (getTagsListRoute , list selectTags tagToResponse)
  -- , (getCategoriesListRoute, list categoryNestedToResponse)
                                                       ]
createRoutes :: [(Route, Handler)]
createRoutes =
  [
  -- ( createAuthorRoute
--     , checkPermission Admin $ create requestToAuthor authorToResponse
--     )
   (createUserRoute, create insertUser requestToUser userToResponse)
--   , (createTagRoute , checkPermission Admin $ create requestToTag tagToResponse)
--   , ( createCategoryRoute
--     , checkPermission Admin $ create requestToCategory categoryNestedToResponse
--     )
--   , (createNewsDraftRoute , create requestToNews newsToResponse)
--   , (createCommentaryRoute, createCommentaryHandler)
                                                                    ]

removeRoutes :: [(Route, Handler)]
removeRoutes =
  [
  -- (deleteAuthorRoute, checkPermission Admin $ remove (Proxy :: Proxy Author))
   (deleteUserRoute, checkPermission Admin $ remove deleteUserById)
--   , (deleteTagRoute   , checkPermission Admin $ remove (Proxy :: Proxy Tag))
--   , ( deleteCategoryRoute
--     , checkPermission Admin $ remove (Proxy :: Proxy Category)
--     )
--   , ( deleteNewsRoute
--     , checkPermission (Owner isAuthorOfNews) $ remove (Proxy :: Proxy News)
--     )
--   , ( deleteCommentRoute
--     , checkPermission (Owner isAuthorOfCommentary)
--       $ remove (Proxy :: Proxy Commentary)
--     )
                                                                   ]
