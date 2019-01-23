{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer.Author where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Database.Models.Author
import           Database.Models.User
import           Data.Functor.Identity

data CreateAuthorRequest = CreateAuthorRequest {
  createAuthorRequestName :: T.Text,
  createAuthorRequestSurname :: T.Text,
  createAuthorRequestAvatar :: T.Text,
  createAuthorRequestDescription :: T.Text
}

instance FromJSON CreateAuthorRequest where
  parseJSON = withObject "CreateAuthorRequest" $ \v ->
    CreateAuthorRequest
      <$> v
      .:  "name"
      <*> v
      .:  "surname"
      <*> v
      .:  "avatar"
      <*> v
      .:  "description"

data CreateAuthorResponse = CreateAuthorResponse {
  createAuthorResponseName :: T.Text,
  createAuthorResponseSurname :: T.Text,
  createAuthorResponseAvatar :: T.Text,
  createAuthorResponseDescription :: T.Text,
  createAuthorResponseId :: Integer,
  createAuthorResponseUserId :: Integer,
  createAuthorResponseDateCreated :: LocalTime,
  createAuthorResponseIsAdmin :: Bool
}

instance ToJSON CreateAuthorResponse where
  toJSON (CreateAuthorResponse name surname avatar desc id uid date isAdmin) =
    object
      [ "name" .= name
      , "surname" .= surname
      , "avatar" .= avatar
      , "description" .= desc
      , "author_id" .= id
      , "user_id" .= uid
      , "date_created" .= date
      , "is_admin" .= isAdmin
      ]

requestToAuthor :: CreateAuthorRequest -> (UserRaw, AuthorRaw)
requestToAuthor CreateAuthorRequest {..} =
  ( UserRaw { userRawName    = createAuthorRequestName
            , userRawSurname = createAuthorRequestSurname
            , userRawAvatar  = createAuthorRequestAvatar
            }
  , AuthorRaw { authorRawDescription = createAuthorRequestDescription }
  )

authorToResponse :: (User, Author) -> CreateAuthorResponse
authorToResponse (User {..}, Author {..}) = CreateAuthorResponse
  { createAuthorResponseName        = userName
  , createAuthorResponseSurname     = userSurname
  , createAuthorResponseAvatar      = userAvatar
  , createAuthorResponseDescription = authorDescription
  , createAuthorResponseId          = authorId
  , createAuthorResponseUserId      = userId
  , createAuthorResponseDateCreated = userDateCreated
  , createAuthorResponseIsAdmin     = userIsAdmin
  }
