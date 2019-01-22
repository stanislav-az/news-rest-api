{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer where

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

data UserRequestT f = UserRequestT {
  urName :: f T.Text,
  urSurname :: f T.Text,
  urAvatar :: f T.Text
}

newtype CreateUserRequest = CreateUserRequest (UserRequestT Identity)

newtype UpdateUserRequest = UpdateUserRequest (UserRequestT Maybe)

newtype CreateUserResponse = CreateUserResponse User

instance ToJSON CreateUserResponse where
  toJSON (CreateUserResponse User {..}) = object
    [ "user_id" .= userId
    , "name" .= userName
    , "surname" .= userSurname
    , "avatar" .= userAvatar
    , "date_created" .= userDateCreated
    , "is_admin" .= userIsAdmin
    ]

instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \v ->
    fmap CreateUserRequest
      $   UserRequestT
      <$> (Identity <$> (v .: "name"))
      <*> (Identity <$> (v .: "surname"))
      <*> (Identity <$> (v .: "avatar"))

instance FromJSON UpdateUserRequest where
  parseJSON = withObject "UpdateUserRequest" $ \v ->
    fmap UpdateUserRequest
      $   UserRequestT
      <$> v .:? "name"
      <*> v .:? "surname"
      <*> v .:? "avatar"

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

requestToUser :: CreateUserRequest -> UserRaw
requestToUser (CreateUserRequest UserRequestT {..}) =
  UserRaw (runIdentity urName) (runIdentity urSurname) (runIdentity urAvatar)

requestToUpdateUser :: UpdateUserRequest -> UserRawPartial
requestToUpdateUser (UpdateUserRequest UserRequestT {..}) =
  UserRawPartial urName urSurname urAvatar

userToResponse :: User -> CreateUserResponse
userToResponse = CreateUserResponse
