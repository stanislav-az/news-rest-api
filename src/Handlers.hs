{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Database
import           Serializer
import           Data.Aeson


type Handler = Request -> IO Response

createAuthorHandler :: Handler
createAuthorHandler req = do
  body <- requestBody req
  let createAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String CreateAuthorRequest
  either reportParseError createAuthor createAuthorData
 where
  createAuthor authorData = do
    (user, author) <- addAuthorToDB $ requestToAuthor authorData
    let authorJSON = encode $ authorToResponse (user, author)
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       authorJSON
  reportParseError err = pure $ responseLBS status400
                                            [("Content-Type", "plain/text")]
                                            ("Parse error: " <> BC.pack err)

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
  either reportParseError createUser createUserData
 where
  createUser userData = do
    user <- addUserToDB $ requestToUser userData
    let userJSON = encode $ userToResponse user
    pure $ responseLBS status200 [("Content-Type", "application/json")] userJSON
  reportParseError err = pure $ responseLBS status400
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
