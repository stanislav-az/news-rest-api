{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.User where

import qualified Data.Aeson                    as JSON
                                                ( FromJSON(..)
                                                , ToJSON(..)
                                                , withObject
                                                , object
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                )
import           Database.Models.User           ( User(..)
                                                , UserRaw(..)
                                                )

newtype CreateUserRequest = CreateUserRequest UserRaw

instance JSON.FromJSON CreateUserRequest where
  parseJSON = JSON.withObject "CreateUserRequest" $ \v ->
    fmap CreateUserRequest
      $   UserRaw
      <$> v
      .:  "name"
      <*> v
      .:  "surname"
      <*> v
      .:  "avatar"

newtype CreateUserResponse = CreateUserResponse User

instance JSON.ToJSON CreateUserResponse where
  toJSON (CreateUserResponse User {..}) = JSON.object
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
