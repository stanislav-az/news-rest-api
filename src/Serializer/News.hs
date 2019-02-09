{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.News where

import           Data.Aeson
import           Data.Functor.Identity
import           Database.Models.News
import           Serializer.Author
import           Serializer.Category
import           Serializer.Tag

newtype CreateNewsRequest = CreateNewsRequest (NewsRawT Identity)

instance FromJSON CreateNewsRequest where
  parseJSON = withObject "CreateNewsRequest" $ \v ->
    fmap CreateNewsRequest
      $   NewsRawT
      <$> (Identity <$> (v .: "title"))
      <*> (Identity <$> (v .: "author_id"))
      <*> (Identity <$> (v .: "category_id"))
      <*> (Identity <$> (v .: "content"))
      <*> (Identity <$> (v .: "main_photo"))
      <*> (Identity <$> (v .: "tags"))
      <*> (Identity <$> (v .: "photos"))

newtype UpdateNewsRequest = UpdateNewsRequest (NewsRawT Maybe)

instance FromJSON UpdateNewsRequest where
  parseJSON = withObject "UpdateNewsRequest" $ \v ->
    fmap UpdateNewsRequest
      $   NewsRawT
      <$> v
      .:? "title"
      <*> v
      .:? "author_id"
      <*> v
      .:? "category_id"
      <*> v
      .:? "content"
      <*> v
      .:? "main_photo"
      <*> v
      .:? "tags"
      <*> v
      .:? "photos"

newtype CreateNewsResponse = CreateNewsResponse NewsNested

instance ToJSON CreateNewsResponse where
  toJSON (CreateNewsResponse NewsNested {..}) = object
    [ "id" .= newsNestedId
    , "title" .= newsNestedTitle
    , "date_created" .= newsNestedDateCreated
    , "content" .= newsNestedContent
    , "main_photo" .= newsNestedMainPhoto
    , "is_draft" .= newsNestedIsDraft
    , "author" .= toJSON (authorToResponse newsNestedAuthorAndUser)
    , "category" .= toJSON (CreateCategoryResponse newsNestedCategory)
    , "tags" .= toJSON (CreateTagResponse <$> newsNestedTags)
    , "photos" .= toJSON (PhotoResponse <$> newsNestedPhotos)
    ]

newtype PhotoResponse = PhotoResponse Photo

instance ToJSON PhotoResponse where
  toJSON (PhotoResponse Photo {..}) =
    object ["id" .= photoId, "url" .= photoUrl, "news_id" .= photoNewsId]

requestToNews :: CreateNewsRequest -> NewsRaw
requestToNews (CreateNewsRequest nrt) = NewsRaw nrt

requestToUpdateNews :: UpdateNewsRequest -> NewsRawPartial
requestToUpdateNews (UpdateNewsRequest nrt) = NewsRawPartial nrt

newsToResponse :: NewsNested -> CreateNewsResponse
newsToResponse = CreateNewsResponse
