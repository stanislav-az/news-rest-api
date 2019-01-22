{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Serializer
import           Data.Aeson
import           Database.Queries.Author
import           Database.Queries.User

type Handler = Request -> IO Response

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

getUidFromUrl :: [T.Text] -> Either String T.Text
getUidFromUrl ["api", "user", uid] = Right uid
getUidFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

updateUserHandler :: Handler
updateUserHandler req = do
  body <- requestBody req
  let updateUserData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateUserRequest
      userId = either error id (getUidFromUrl $ pathInfo req)

  either (pure . reportParseError) (goUpdateUser userId) updateUserData
 where
  goUpdateUser :: T.Text -> UpdateUserRequest -> IO Response
  goUpdateUser uid userData = do
    let partial = requestToUpdateUser userData
    user <- updateUser uid partial
    let userJSON = encode $ userToResponse user
    pure $ responseLBS status200 [("Content-Type", "application/json")] userJSON
