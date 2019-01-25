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
import           Database.Queries.Author
import           Database.Queries.User
import           Database.Queries.Tag

type Handler = Request -> IO Response

getAuthorIdFromUrl :: [T.Text] -> Either String T.Text
getAuthorIdFromUrl ["api", "author", authorId] = Right authorId
getAuthorIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

updateAuthorHandler :: Handler
updateAuthorHandler req = do
  body <- requestBody req
  let updateAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateAuthorRequest
      authorId = either error id (getAuthorIdFromUrl $ pathInfo req)

  either (pure . reportParseError) (goUpdateAuthor authorId) updateAuthorData
 where
  goUpdateAuthor :: T.Text -> UpdateAuthorRequest -> IO Response
  goUpdateAuthor authorId authorData = do
    let partial = requestToUpdateAuthor authorData
    author <- updateAuthor authorId partial
    let authorJSON = encode $ authorToUpdateResponse author
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       authorJSON

createAuthorHandler :: Handler
createAuthorHandler req = do
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
getAuthorsListHandler req = do
  usersAndAuthors <- getAuthorsList
  let authors          = authorToResponse <$> usersAndAuthors
      printableAuthors = encode authors
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableAuthors

createUserHandler :: Handler
createUserHandler req = do
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
getUsersListHandler req = do
  usersDB <- getUsersList
  let users          = userToResponse <$> usersDB
      printableUsers = encode users
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableUsers

getUserIdFromUrl :: [T.Text] -> Either String T.Text
getUserIdFromUrl ["api", "user", userId] = Right userId
getUserIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

updateUserHandler :: Handler
updateUserHandler req = do
  body <- requestBody req
  let updateUserData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateUserRequest
      userId = either error id (getUserIdFromUrl $ pathInfo req)

  either (pure . reportParseError) (goUpdateUser userId) updateUserData
 where
  goUpdateUser :: T.Text -> UpdateUserRequest -> IO Response
  goUpdateUser uid userData = do
    let partial = requestToUpdateUser userData
    user <- updateUser uid partial
    let userJSON = encode $ userToResponse user
    pure $ responseLBS status200 [("Content-Type", "application/json")] userJSON

-- Tag

createTagHandler :: Handler
createTagHandler req = do
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
getTagsListHandler req = do
  tagsDB <- getTagsList
  let tags          = tagToResponse <$> tagsDB
      printableTags = encode tags
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableTags

getTagIdFromUrl :: [T.Text] -> Either String T.Text
getTagIdFromUrl ["api", "tag", tagId] = Right tagId
getTagIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

updateTagHandler :: Handler
updateTagHandler req = do
  body <- requestBody req
  let updateTagData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateTagRequest
      tagId = either error id (getTagIdFromUrl $ pathInfo req)

  either (pure . reportParseError) (goUpdateTag tagId) updateTagData
 where
  goUpdateTag :: T.Text -> UpdateTagRequest -> IO Response
  goUpdateTag uid tagData = do
    let partial = requestToUpdateTag tagData
    tag <- updateTag uid partial
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS status200 [("Content-Type", "application/json")] tagJSON
