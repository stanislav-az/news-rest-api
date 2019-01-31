{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
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
import           Data.Maybe                     ( fromMaybe )

-- dynamic path information type and value pairs
type DynamicPathsMap = [(T.Text, T.Text)]

type Handler = DynamicPathsMap -> Request -> IO Response

getIdFromUrl :: DynamicPathsMap -> Either String Integer
getIdFromUrl dpMap =
  (maybe (Left "no info") Right $ lookup "id" dpMap) >>= textToInteger

updateAuthorHandler :: Handler
updateAuthorHandler dpMap req = do
  body <- requestBody req
  let updateAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateAuthorRequest
      authorId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap

  either (pure . reportParseError) (goUpdateAuthor authorId) updateAuthorData
 where
  goUpdateAuthor :: Integer -> UpdateAuthorRequest -> IO Response
  goUpdateAuthor authorId authorData = do
    let partial = requestToUpdateAuthor authorData
    author <- updateAuthor authorId partial
    let authorJSON = encode $ authorToUpdateResponse author
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       authorJSON

createAuthorHandler :: Handler
createAuthorHandler dpMap req = do
  body <- requestBody req
  let createAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String CreateAuthorRequest
  either (pure . reportParseError) createAuthor createAuthorData
 where
  createAuthor authorData = do
    (user, author) <- addAuthorToDB $ requestToAuthor authorData
    let authorJSON = encode $ authorToResponse (user, author)
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       authorJSON

getAuthorsListHandler :: Handler
getAuthorsListHandler dpMap req = do
  usersAndAuthors <- getAuthorsList
  let authors          = authorToResponse <$> usersAndAuthors
      printableAuthors = encode authors
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableAuthors

createUserHandler :: Handler
createUserHandler dpMap req = do
  body <- requestBody req
  let createUserData =
        eitherDecode $ LB.fromStrict body :: Either String CreateUserRequest
  either (pure . reportParseError) createUser createUserData
 where
  createUser userData = do
    user <- addUserToDB $ requestToUser userData
    let userJSON = encode $ userToResponse user
    pure $ responseLBS status200 [("Content-Type", "application/json")] userJSON

reportParseError :: String -> Response
reportParseError err = responseLBS status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> BC.pack err)

getUsersListHandler :: Handler
getUsersListHandler dpMap req = do
  usersDB <- getUsersList
  let users          = userToResponse <$> usersDB
      printableUsers = encode users
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableUsers

-- Tag

createTagHandler :: Handler
createTagHandler dpMap req = do
  body <- requestBody req
  let createTagData =
        eitherDecode $ LB.fromStrict body :: Either String CreateTagRequest
  either (pure . reportParseError) createTag createTagData
 where
  createTag tagData = do
    tag <- addTagToDB $ requestToTag tagData
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS status200 [("Content-Type", "application/json")] tagJSON

getTagsListHandler :: Handler
getTagsListHandler dpMap req = do
  tagsDB <- getTagsList
  let tags          = tagToResponse <$> tagsDB
      printableTags = encode tags
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableTags

updateTagHandler :: Handler
updateTagHandler dpMap req = do
  body <- requestBody req
  let updateTagData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateTagRequest
      tagId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap

  either (pure . reportParseError) (goUpdateTag tagId) updateTagData
 where
  goUpdateTag :: Integer -> UpdateTagRequest -> IO Response
  goUpdateTag tagId tagData = do
    let partial = requestToUpdateTag tagData
    tag <- updateTag tagId partial
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS status200 [("Content-Type", "application/json")] tagJSON

-- Category

createCategoryHandler :: Handler
createCategoryHandler dpMap req = do
  body <- requestBody req
  let createCategoryData =
        eitherDecode $ LB.fromStrict body :: Either String CreateCategoryRequest
  either (pure . reportParseError) createCategory createCategoryData
 where
  createCategory categoryData = do
    category <- addCategoryToDB $ requestToCategory categoryData
    let categoryJSON = encode $ categoryNestedToResponse category
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       categoryJSON

getCategoriesListHandler :: Handler
getCategoriesListHandler dpMap req = do
  categoriesDB <- getCategoriesList
  let categories          = categoryNestedToResponse <$> categoriesDB
      printableCategories = encode categories
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableCategories

updateCategoryHandler :: Handler
updateCategoryHandler dpMap req = do
  body <- requestBody req
  let
    updateCategoryData =
      eitherDecode $ LB.fromStrict body :: Either String UpdateCategoryRequest
    categoryId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
      $ getIdFromUrl dpMap

  either (pure . reportParseError)
         (goUpdateCategory categoryId)
         updateCategoryData
 where
  goUpdateCategory :: Integer -> UpdateCategoryRequest -> IO Response
  goUpdateCategory categoryId categoryData = do
    let partial = requestToUpdateCategory categoryData
    category <- updateCategory categoryId partial
    let categoryJSON = encode $ categoryNestedToResponse category
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       categoryJSON

--News

createNewsDraftHandler :: Handler
createNewsDraftHandler dpMap req = do
  body <- requestBody req
  let createNewsDraftData =
        eitherDecode $ LB.fromStrict body :: Either String CreateNewsRequest
  either (pure . reportParseError) createNewsDraft createNewsDraftData
 where
  createNewsDraft newsData = do
    news <- addNewsToDB $ requestToNews newsData
    let newsJSON = encode $ newsToResponse news
    pure $ responseLBS status200 [("Content-Type", "application/json")] newsJSON

getNewsListHandler :: Handler
getNewsListHandler dpMap req = do
  newsDB <- getNewsList
  let news          = newsToResponse <$> newsDB
      printableNews = encode news
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableNews

updateNewsHandler :: Handler
updateNewsHandler dpMap req = do
  body <- requestBody req
  let updateNewsData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateNewsRequest
      newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap

  either (pure . reportParseError) (goUpdateNews newsId) updateNewsData
 where
  goUpdateNews :: Integer -> UpdateNewsRequest -> IO Response
  goUpdateNews newsId newsData = do
    let partial = requestToUpdateNews newsData
    news <- updateNews newsId partial
    let newsJSON = encode $ newsToResponse news
    pure $ responseLBS status200 [("Content-Type", "application/json")] newsJSON

publishNewsHandler :: Handler
publishNewsHandler dpMap req = do
  body <- requestBody req
  let newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  news <- publishNews newsId
  let newsJSON = encode $ newsToResponse news
  pure $ responseLBS status200 [("Content-Type", "application/json")] newsJSON
