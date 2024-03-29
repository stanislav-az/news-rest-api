{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module News.Routes where

import Control.Monad.Except
import Control.Monad.Reader
import News.Handlers
import News.Middlewares
import qualified Network.Wai as W (Response(..))
import News.Serializer.Author (authorToResponse, requestToAuthor)
import News.Serializer.Category (categoryNestedToResponse, requestToCategory)
import News.Serializer.News (newsToResponse, requestToNews)
import News.Serializer.Tag (requestToTag, tagToResponse)
import News.Serializer.User (requestToUser, userToResponse)
import News.WebServer.HandlerClass (MonadHTTP(..))
import News.WebServer.HandlerMonad
  ( Handler(..)
  , HandlerEnv(..)
  , HandlerError(..)
  , okResponseWithJSONBody
  )
import News.WebServer.MonadDatabase
  ( Authorization(..)
  , PersistentAuthor(..)
  , PersistentCategory(..)
  , PersistentCommentary(..)
  , PersistentNews(..)
  , PersistentTag(..)
  , PersistentUser(..)
  )
import News.WebServer.Router (Route(..))

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "POST"

updateAuthorRoute :: Route
updateAuthorRoute =
  PathRoute "api" $
  PathRoute "authors" $ DynamicRoute "id" $ MethodRoute "PATCH"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

deleteAuthorRoute :: Route
deleteAuthorRoute =
  PathRoute "api" $
  PathRoute "authors" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteTagRoute :: Route
deleteTagRoute =
  PathRoute "api" $ PathRoute "tags" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteNewsRoute :: Route
deleteNewsRoute =
  PathRoute "api" $ PathRoute "posts" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteCategoryRoute :: Route
deleteCategoryRoute =
  PathRoute "api" $
  PathRoute "categories" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteUserRoute :: Route
deleteUserRoute =
  PathRoute "api" $ PathRoute "users" $ DynamicRoute "id" $ MethodRoute "DELETE"

deleteCommentRoute :: Route
deleteCommentRoute =
  PathRoute "api" $
  PathRoute "comments" $ DynamicRoute "id" $ MethodRoute "DELETE"

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
  PathRoute "api" $
  PathRoute "categories" $ DynamicRoute "id" $ MethodRoute "PATCH"

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
  PathRoute "api" $
  PathRoute "posts" $
  PathRoute "search" $ DynamicRoute "search" $ MethodRoute "GET"

createCommentaryRoute :: Route
createCommentaryRoute =
  PathRoute "api" $
  PathRoute "posts" $
  DynamicRoute "id" $ PathRoute "comments" $ MethodRoute "POST"

getCommentariesListRoute :: Route
getCommentariesListRoute =
  PathRoute "api" $
  PathRoute "posts" $
  DynamicRoute "id" $ PathRoute "comments" $ MethodRoute "GET"

routes :: [(Route, Handler)]
routes =
  [ (getNewsListRoute, listNews)
  , (searchNewsRoute, searchNews)
  , (updateNewsRoute, checkPermission (Owner isAuthorOfNews) updateNewsHandler)
  , ( publishNewsRoute
    , checkPermission (Owner isAuthorOfNews) publishNewsHandler)
  , (getCommentariesListRoute, listCommentariesHandler)
  , (MethodRoute "GET", okResponseWithJSONBody "{\"ok\": true}")
  , ( deleteNewsRoute
    , checkPermission (Owner isAuthorOfNews) $ remove deleteNewsById)
  , (createNewsDraftRoute, create insertNews requestToNews newsToResponse)
  , (createCommentaryRoute, createCommentaryHandler)
  ] ++
  listRoutes ++ createRoutes ++ removeRoutes ++ updateRoutes

updateRoutes :: [(Route, Handler)]
updateRoutes =
  [ (updateAuthorRoute, checkPermission Admin updateAuthorHandler)
  , (updateTagRoute, checkPermission Admin updateTagHandler)
  , (updateCategoryRoute, checkPermission Admin updateCategoryHandler)
  ]

listRoutes ::
     ( MonadReader HandlerEnv m
     , MonadError HandlerError m
     , MonadHTTP m
     , PersistentUser m
     , PersistentAuthor m
     , PersistentTag m
     , PersistentCategory m
     )
  => [(Route, m W.Response)]
listRoutes =
  [ (getUsersListRoute, list selectUsers userToResponse)
  , ( getAuthorsListRoute
    , checkPermission Admin $ list selectAuthors authorToResponse)
  , (getTagsListRoute, list selectTags tagToResponse)
  , ( getCategoriesListRoute
    , list selectCategoriesNested categoryNestedToResponse)
  ]

createRoutes ::
     ( MonadReader HandlerEnv m
     , MonadError HandlerError m
     , MonadHTTP m
     , PersistentUser m
     , PersistentAuthor m
     , PersistentTag m
     , PersistentCategory m
     )
  => [(Route, m W.Response)]
createRoutes =
  [ (createUserRoute, create insertUser requestToUser userToResponse)
  , ( createAuthorRoute
    , checkPermission Admin $
      create insertAuthor requestToAuthor authorToResponse)
  , ( createTagRoute
    , checkPermission Admin $ create insertTag requestToTag tagToResponse)
  , ( createCategoryRoute
    , checkPermission Admin $
      create insertCategory requestToCategory categoryNestedToResponse)
  ]

removeRoutes ::
     ( MonadReader HandlerEnv m
     , MonadError HandlerError m
     , MonadHTTP m
     , PersistentUser m
     , PersistentAuthor m
     , PersistentTag m
     , PersistentCategory m
     , PersistentCommentary m
     , Authorization m
     )
  => [(Route, m W.Response)]
removeRoutes =
  [ (deleteUserRoute, checkPermission Admin $ remove deleteUserById)
  , (deleteAuthorRoute, checkPermission Admin $ remove deleteAuthorById)
  , (deleteTagRoute, checkPermission Admin $ remove deleteTagById)
  , (deleteCategoryRoute, checkPermission Admin $ remove deleteCategoryById)
  , ( deleteCommentRoute
    , checkPermission (Owner isAuthorOfCommentary) $ remove deleteCommentaryById)
  ]
