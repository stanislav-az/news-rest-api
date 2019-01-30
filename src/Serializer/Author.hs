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

newtype UpdateAuthorRequest = UpdateAuthorRequest AuthorRaw

instance FromJSON UpdateAuthorRequest where
  parseJSON = withObject "UpdateAuthorRequest"
    $ \v -> UpdateAuthorRequest . AuthorRaw <$> v .: "description"

data CreateAuthorResponse = CreateAuthorResponse {
  createAuthorResponseName :: T.Text,
  createAuthorResponseSurname :: T.Text,
  createAuthorResponseAvatar :: T.Text,
  createAuthorResponseDescription :: T.Text,
  createAuthorResponseUserId :: Integer,
  createAuthorResponseDateCreated :: LocalTime,
  createAuthorResponseIsAdmin :: Bool
}

instance ToJSON CreateAuthorResponse where
  toJSON CreateAuthorResponse {..} = object
    [ "name" .= createAuthorResponseName
    , "surname" .= createAuthorResponseSurname
    , "avatar" .= createAuthorResponseAvatar
    , "description" .= createAuthorResponseDescription
    , "user_id" .= createAuthorResponseUserId
    , "date_created" .= createAuthorResponseDateCreated
    , "is_admin" .= createAuthorResponseIsAdmin
    ]

newtype UpdateAuthorResponse = UpdateAuthorResponse Author

instance ToJSON UpdateAuthorResponse where
  toJSON (UpdateAuthorResponse Author {..}) =
    object ["user_id" .= authorUserId, "description" .= authorDescription]

requestToAuthor :: CreateAuthorRequest -> (UserRaw, AuthorRaw)
requestToAuthor CreateAuthorRequest {..} =
  ( UserRaw { userRawName    = createAuthorRequestName
            , userRawSurname = createAuthorRequestSurname
            , userRawAvatar  = createAuthorRequestAvatar
            }
  , AuthorRaw { authorRawDescription = createAuthorRequestDescription }
  )

requestToUpdateAuthor :: UpdateAuthorRequest -> AuthorRaw
requestToUpdateAuthor (UpdateAuthorRequest author) = author

authorToResponse :: (User, Author) -> CreateAuthorResponse
authorToResponse (User {..}, Author {..}) = CreateAuthorResponse
  { createAuthorResponseName        = userName
  , createAuthorResponseSurname     = userSurname
  , createAuthorResponseAvatar      = userAvatar
  , createAuthorResponseDescription = authorDescription
  , createAuthorResponseUserId      = userId
  , createAuthorResponseDateCreated = userDateCreated
  , createAuthorResponseIsAdmin     = userIsAdmin
  }

authorToUpdateResponse :: Author -> UpdateAuthorResponse
authorToUpdateResponse = UpdateAuthorResponse
