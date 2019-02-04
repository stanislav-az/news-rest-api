{-# LANGUAGE OverloadedStrings #-}
module Routes where

import           WebServer.Router
import           Network.Wai
import           Network.HTTP.Types
import           Handlers
import           Middlewares
import           Database.Queries.News
import           WebServer.MonadHandler
import           Database.Models.Author
import           Database.Models.News
import           Database.Models.Category
import           Database.Models.User
import           Database.Models.Tag
import           Data.Proxy
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
  [ ( createAuthorRoute
    , checkPermission Admin $ create requestToAuthor authorToResponse
    )
  , (updateAuthorRoute  , checkPermission Admin updateAuthorHandler)
  , (getAuthorsListRoute, checkPermission Admin $ list authorToResponse)
  , (deleteAuthorRoute, checkPermission Admin $ remove (Proxy :: Proxy Author))
  , (createUserRoute    , create requestToUser userToResponse)
  , (getUsersListRoute  , list userToResponse)
  , (deleteUserRoute, checkPermission Admin $ remove (Proxy :: Proxy User))
  , (createTagRoute, checkPermission Admin $ create requestToTag tagToResponse)
  , (updateTagRoute     , checkPermission Admin updateTagHandler)
  , (deleteTagRoute, checkPermission Admin $ remove (Proxy :: Proxy Tag))
  , (getTagsListRoute   , list tagToResponse)
  , ( createCategoryRoute
    , checkPermission Admin $ create requestToCategory categoryNestedToResponse
    )
  , (updateCategoryRoute, checkPermission Admin updateCategoryHandler)
  , ( deleteCategoryRoute
    , checkPermission Admin $ remove (Proxy :: Proxy Category)
    )
  , (getCategoriesListRoute, list categoryNestedToResponse)
  , (getNewsListRoute      , list newsToResponse)
  , (createNewsDraftRoute  , create requestToNews newsToResponse)
  , (updateNewsRoute, checkPermission (Owner isAuthorOfNews) updateNewsHandler)
  , ( publishNewsRoute
    , checkPermission (Owner isAuthorOfNews) publishNewsHandler
    )
  , ( deleteNewsRoute
    , checkPermission (Owner isAuthorOfNews) $ remove (Proxy :: Proxy News)
    )
  , (createCommentaryRoute   , createCommentaryHandler)
  , (getCommentariesListRoute, listCommentariesHandler)
  , ( MethodRoute "GET"
    , pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]
