{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.User where

import           Data.Aeson
import           Database.Models.User

newtype CreateUserRequest = CreateUserRequest UserRaw

instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \v ->
    fmap CreateUserRequest
      $   UserRaw
      <$> v
      .:  "name"
      <*> v
      .:  "surname"
      <*> v
      .:  "avatar"

newtype CreateUserResponse = CreateUserResponse User

instance ToJSON CreateUserResponse where
  toJSON (CreateUserResponse User {..}) = object
    [ "id" .= userId
    , "name" .= userName
    , "surname" .= userSurname
    , "avatar" .= userAvatar
    , "date_created" .= userDateCreated
    , "is_admin" .= userIsAdmin
    ]

requestToUser :: CreateUserRequest -> UserRaw
requestToUser (CreateUserRequest user) = user

userToResponse :: User -> CreateUserResponse
userToResponse = CreateUserResponse
