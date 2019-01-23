{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer.User where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Database.Models.User
import           Data.Functor.Identity

data UserRequestT f = UserRequestT {
  urName :: f T.Text,
  urSurname :: f T.Text,
  urAvatar :: f T.Text
}

newtype CreateUserRequest = CreateUserRequest (UserRequestT Identity)

instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \v ->
    fmap CreateUserRequest
      $   UserRequestT
      <$> (Identity <$> (v .: "name"))
      <*> (Identity <$> (v .: "surname"))
      <*> (Identity <$> (v .: "avatar"))

newtype UpdateUserRequest = UpdateUserRequest (UserRequestT Maybe)

instance FromJSON UpdateUserRequest where
  parseJSON = withObject "UpdateUserRequest" $ \v ->
    fmap UpdateUserRequest
      $   UserRequestT
      <$> v
      .:? "name"
      <*> v
      .:? "surname"
      <*> v
      .:? "avatar"

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

requestToUser :: CreateUserRequest -> UserRaw
requestToUser (CreateUserRequest UserRequestT {..}) =
  UserRaw (runIdentity urName) (runIdentity urSurname) (runIdentity urAvatar)

requestToUpdateUser :: UpdateUserRequest -> UserRawPartial
requestToUpdateUser (UpdateUserRequest UserRequestT {..}) =
  UserRawPartial urName urSurname urAvatar

userToResponse :: User -> CreateUserResponse
userToResponse = CreateUserResponse
