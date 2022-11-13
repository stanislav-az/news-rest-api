{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module News.Serializer.Author where

import qualified Data.Aeson as JSON
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  )
import Data.Aeson ((.:), (.=))
import qualified Data.Text as T (Text(..))
import qualified Data.Time as Time (LocalTime(..))
import News.Database.Models.Author (Author(..), AuthorRaw(..))
import News.Database.Models.User (User(..), UserRaw(..))

data CreateAuthorRequest = CreateAuthorRequest
  { createAuthorRequestName :: T.Text
  , createAuthorRequestSurname :: T.Text
  , createAuthorRequestAvatar :: T.Text
  , createAuthorRequestDescription :: T.Text
  }

newtype UpdateAuthorRequest =
  UpdateAuthorRequest AuthorRaw

data CreateAuthorResponse = CreateAuthorResponse
  { createAuthorResponseName :: T.Text
  , createAuthorResponseSurname :: T.Text
  , createAuthorResponseAvatar :: T.Text
  , createAuthorResponseDescription :: T.Text
  , createAuthorResponseId :: Integer
  , createAuthorResponseUserId :: Integer
  , createAuthorResponseDateCreated :: Time.LocalTime
  , createAuthorResponseIsAdmin :: Bool
  }

newtype UpdateAuthorResponse =
  UpdateAuthorResponse Author

instance JSON.FromJSON CreateAuthorRequest where
  parseJSON =
    JSON.withObject "CreateAuthorRequest" $ \v ->
      CreateAuthorRequest <$> v .: "name" <*> v .: "surname" <*> v .: "avatar" <*>
      v .: "description"

instance JSON.FromJSON UpdateAuthorRequest where
  parseJSON =
    JSON.withObject "UpdateAuthorRequest" $ \v ->
      UpdateAuthorRequest . AuthorRaw <$> v .: "description"

instance JSON.ToJSON CreateAuthorResponse where
  toJSON CreateAuthorResponse {..} =
    JSON.object
      [ "name" .= createAuthorResponseName
      , "surname" .= createAuthorResponseSurname
      , "avatar" .= createAuthorResponseAvatar
      , "description" .= createAuthorResponseDescription
      , "id" .= createAuthorResponseId
      , "user_id" .= createAuthorResponseUserId
      , "date_created" .= createAuthorResponseDateCreated
      , "is_admin" .= createAuthorResponseIsAdmin
      ]

instance JSON.ToJSON UpdateAuthorResponse where
  toJSON (UpdateAuthorResponse Author {..}) =
    JSON.object
      [ "id" .= authorId
      , "user_id" .= authorUserId
      , "description" .= authorDescription
      ]

requestToAuthor :: CreateAuthorRequest -> (AuthorRaw, UserRaw)
requestToAuthor CreateAuthorRequest {..} =
  ( AuthorRaw {authorRawDescription = createAuthorRequestDescription}
  , UserRaw
      { userRawName = createAuthorRequestName
      , userRawSurname = createAuthorRequestSurname
      , userRawAvatar = createAuthorRequestAvatar
      })

requestToUpdateAuthor :: UpdateAuthorRequest -> AuthorRaw
requestToUpdateAuthor (UpdateAuthorRequest author) = author

authorToResponse :: (Author, User) -> CreateAuthorResponse
authorToResponse (Author {..}, User {..}) =
  CreateAuthorResponse
    { createAuthorResponseName = userName
    , createAuthorResponseSurname = userSurname
    , createAuthorResponseAvatar = userAvatar
    , createAuthorResponseDescription = authorDescription
    , createAuthorResponseId = authorId
    , createAuthorResponseUserId = userId
    , createAuthorResponseDateCreated = userDateCreated
    , createAuthorResponseIsAdmin = userIsAdmin
    }

authorToUpdateResponse :: Author -> UpdateAuthorResponse
authorToUpdateResponse = UpdateAuthorResponse
