{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import qualified Data.ByteString               as B
import           Network.Wai
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Network.HTTP.Types            as HTTP
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Data.Aeson
import           Serializer.User
import           Serializer.Author
import           Serializer.Tag
import           Serializer.Category
import           Serializer.News
import           Database.Queries.Author
import           Database.Queries.User
import           Database.Queries.Tag
import           Database.Queries.Category
import           Database.Queries.News
import           Helpers
import           WebServer.MonadHandler
import           Control.Monad.Reader
import           Data.Maybe                     ( fromMaybe )

--type Handler = DynamicPathsMap -> Request -> IO Response

getIdFromUrl :: DynamicPathsMap -> Either String Integer
getIdFromUrl dpMap =
  (maybe (Left "no info") Right $ lookup "id" dpMap) >>= textToInteger

updateAuthorHandler :: Handler
updateAuthorHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- liftIO $ requestBody req
  let updateAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateAuthorRequest
      authorId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError)
                  (goUpdateAuthor conn authorId)
                  updateAuthorData
 where
  goUpdateAuthor
    :: PSQL.Connection -> Integer -> UpdateAuthorRequest -> IO Response
  goUpdateAuthor conn authorId authorData = do
    let partial = requestToUpdateAuthor authorData
    author <- updateAuthor conn authorId partial
    let authorJSON = encode $ authorToUpdateResponse author
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       authorJSON

createAuthorHandler :: Handler
createAuthorHandler = do
  req  <- asks hRequest
  body <- liftIO $ requestBody req
  let createAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String CreateAuthorRequest
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError) (createAuthor conn) createAuthorData
 where
  createAuthor conn authorData = do
    (user, author) <- addAuthorToDB conn (requestToAuthor authorData)
    let authorJSON = encode $ authorToResponse (user, author)
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       authorJSON

getAuthorsListHandler :: Handler
getAuthorsListHandler = do
  conn            <- asks hConnection
  usersAndAuthors <- liftIO $ getAuthorsList conn
  let authors          = authorToResponse <$> usersAndAuthors
      printableAuthors = encode authors
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     printableAuthors

createUserHandler :: Handler
createUserHandler = do
  req  <- asks hRequest
  body <- liftIO $ requestBody req
  let createUserData =
        eitherDecode $ LB.fromStrict body :: Either String CreateUserRequest
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError) (createUser conn) createUserData
 where
  createUser conn userData = do
    user <- addUserToDB conn (requestToUser userData)
    let userJSON = encode $ userToResponse user
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       userJSON

reportParseError :: String -> Response
reportParseError err = responseLBS HTTP.status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> BC.pack err)

getUsersListHandler :: Handler
getUsersListHandler = do
  conn <- asks hConnection
  usersDB <- liftIO $ getUsersList conn
  let users          = userToResponse <$> usersDB
      printableUsers = encode users
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     printableUsers

-- Tag

createTagHandler :: Handler
createTagHandler = do
  req  <- asks hRequest
  body <- liftIO $ requestBody req
  let createTagData =
        eitherDecode $ LB.fromStrict body :: Either String CreateTagRequest
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError) (createTag conn) createTagData
 where
  createTag conn tagData = do
    tag <- addTagToDB conn $ requestToTag tagData
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       tagJSON

getTagsListHandler :: Handler
getTagsListHandler = do
  conn <- asks hConnection
  tagsDB <- liftIO $ getTagsList conn
  let tags          = tagToResponse <$> tagsDB
      printableTags = encode tags
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     printableTags

updateTagHandler :: Handler
updateTagHandler = do
  req  <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body <- liftIO $ requestBody req
  let updateTagData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateTagRequest
      tagId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError) (goUpdateTag conn tagId) updateTagData
 where
  goUpdateTag :: PSQL.Connection -> Integer -> UpdateTagRequest -> IO Response
  goUpdateTag conn tagId tagData = do
    let partial = requestToUpdateTag tagData
    tag <- updateTag conn tagId partial
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       tagJSON

-- Category

createCategoryHandler :: Handler
createCategoryHandler = do
  req  <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body <- liftIO $ requestBody req
  let createCategoryData =
        eitherDecode $ LB.fromStrict body :: Either String CreateCategoryRequest
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError) (createCategory conn) createCategoryData
 where
  createCategory conn categoryData = do
    category <- addCategoryToDB conn $ requestToCategory categoryData
    let categoryJSON = encode $ categoryNestedToResponse category
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       categoryJSON

getCategoriesListHandler :: Handler
getCategoriesListHandler = do
  conn <- asks hConnection
  categoriesDB <- liftIO $ getCategoriesList conn
  let categories          = categoryNestedToResponse <$> categoriesDB
      printableCategories = encode categories
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     printableCategories

updateCategoryHandler :: Handler
updateCategoryHandler = do
  req  <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body <- liftIO $ requestBody req
  let
    updateCategoryData =
      eitherDecode $ LB.fromStrict body :: Either String UpdateCategoryRequest
    categoryId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
      $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError)
         (goUpdateCategory conn categoryId)
         updateCategoryData
 where
  goUpdateCategory conn categoryId categoryData = do
    let partial = requestToUpdateCategory categoryData
    category <- updateCategory conn categoryId partial
    let categoryJSON = encode $ categoryNestedToResponse category
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       categoryJSON

--News

createNewsDraftHandler :: Handler
createNewsDraftHandler = do
  req  <- asks hRequest
  body <- liftIO $ requestBody req
  let createNewsDraftData =
        eitherDecode $ LB.fromStrict body :: Either String CreateNewsRequest
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError) (createNewsDraft conn) createNewsDraftData
 where
  createNewsDraft conn newsData = do
    news <- addNewsToDB conn $ requestToNews newsData
    let newsJSON = encode $ newsToResponse news
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       newsJSON

getNewsListHandler :: Handler
getNewsListHandler = do
  conn <- asks hConnection
  newsDB <- liftIO $ getNewsList conn
  let news          = newsToResponse <$> newsDB
      printableNews = encode news
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     printableNews

updateNewsHandler :: Handler
updateNewsHandler = do
  req  <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body <- liftIO $ requestBody req
  let updateNewsData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateNewsRequest
      newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError) (goUpdateNews conn newsId) updateNewsData
 where
  goUpdateNews conn newsId newsData = do
    let partial = requestToUpdateNews newsData
    news <- updateNews conn newsId partial
    let newsJSON = encode $ newsToResponse news
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       newsJSON

publishNewsHandler :: Handler
publishNewsHandler = do
  req  <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body <- liftIO $ requestBody req
  let newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  news <- liftIO $ publishNews conn newsId
  let newsJSON = encode $ newsToResponse news
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     newsJSON
