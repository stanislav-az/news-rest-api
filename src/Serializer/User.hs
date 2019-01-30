{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer.User where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Database.Models.User
import           Data.Functor.Identity

newtype CreateUserRequest = CreateUserRequest UserRaw

instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \v ->
    fmap CreateUserRequest $ UserRaw
      <$> v
      .:  "name"
      <*> v
      .:  "surname"
      <*> v
      .:  "avatar"

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
requestToUser (CreateUserRequest user) = user

userToResponse :: User -> CreateUserResponse
userToResponse = CreateUserResponse
