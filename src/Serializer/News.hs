{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer.News where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Database.Models.News
import           Serializer.Author
import           Serializer.Category
import           Serializer.Tag
import           Data.Functor.Identity

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

newtype CreateNewsResponse = CreateNewsResponse NewsNested

instance ToJSON CreateNewsResponse where
  toJSON (CreateNewsResponse NewsNested {..}) = object
    [ "news_id" .= newsNestedId
    , "title" .= newsNestedTitle
    , "date_created" .= newsNestedDateCreated
    , "content" .= newsNestedContent
    , "main_photo" .= newsNestedMainPhoto
    , "is_draft" .= newsNestedIsDraft
    , "author" .= toJSON (AuthorNestedResponse newsNestedAuthor)
    , "category" .= toJSON (CreateCategoryResponse newsNestedCategory)
    , "tags" .= toJSON (CreateTagResponse <$> newsNestedTags)
    ]

requestToNews :: CreateNewsRequest -> NewsRaw
requestToNews (CreateNewsRequest nrt) = NewsRaw nrt

requestToUpdateNews :: UpdateNewsRequest -> NewsRawPartial
requestToUpdateNews (UpdateNewsRequest nrt) = NewsRawPartial nrt

newsToResponse :: NewsNested -> CreateNewsResponse
newsToResponse = CreateNewsResponse
