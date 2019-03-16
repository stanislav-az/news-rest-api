{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.News where

import qualified Data.Aeson as JSON
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  )
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Functor.Identity as I (Identity(..))
import Database.Models.News
  ( NewsNested(..)
  , NewsRaw(..)
  , NewsRawPartial(..)
  , NewsRawT(..)
  , Photo(..)
  )
import Serializer.Author (authorToResponse)
import Serializer.Category (CreateCategoryResponse(..))
import Serializer.Tag (CreateTagResponse(..))

newtype CreateNewsRequest =
  CreateNewsRequest (NewsRawT I.Identity)

newtype UpdateNewsRequest =
  UpdateNewsRequest (NewsRawT Maybe)

newtype CreateNewsResponse =
  CreateNewsResponse NewsNested

newtype PhotoResponse =
  PhotoResponse Photo

instance JSON.FromJSON CreateNewsRequest where
  parseJSON =
    JSON.withObject "CreateNewsRequest" $ \v ->
      fmap CreateNewsRequest $
      NewsRawT <$> (I.Identity <$> (v .: "title")) <*>
      (I.Identity <$> (v .: "author_id")) <*>
      (I.Identity <$> (v .: "category_id")) <*>
      (I.Identity <$> (v .: "content")) <*>
      (I.Identity <$> (v .: "main_photo")) <*>
      (I.Identity <$> (v .: "tags")) <*>
      (I.Identity <$> (v .: "photos"))

instance JSON.FromJSON UpdateNewsRequest where
  parseJSON =
    JSON.withObject "UpdateNewsRequest" $ \v ->
      fmap UpdateNewsRequest $
      NewsRawT <$> v .:? "title" <*> v .:? "author_id" <*> v .:? "category_id" <*>
      v .:? "content" <*>
      v .:? "main_photo" <*>
      v .:? "tags" <*>
      v .:? "photos"

instance JSON.ToJSON CreateNewsResponse where
  toJSON (CreateNewsResponse NewsNested {..}) =
    JSON.object
      [ "id" .= newsNestedId
      , "title" .= newsNestedTitle
      , "date_created" .= newsNestedDateCreated
      , "content" .= newsNestedContent
      , "main_photo" .= newsNestedMainPhoto
      , "is_draft" .= newsNestedIsDraft
      , "author" .= JSON.toJSON (authorToResponse newsNestedAuthorAndUser)
      , "category" .= JSON.toJSON (CreateCategoryResponse newsNestedCategory)
      , "tags" .= JSON.toJSON (CreateTagResponse <$> newsNestedTags)
      , "photos" .= JSON.toJSON (PhotoResponse <$> newsNestedPhotos)
      ]

instance JSON.ToJSON PhotoResponse where
  toJSON (PhotoResponse Photo {..}) =
    JSON.object ["id" .= photoId, "url" .= photoUrl, "news_id" .= photoNewsId]

requestToNews :: CreateNewsRequest -> NewsRaw
requestToNews (CreateNewsRequest nrt) = NewsRaw nrt

requestToUpdateNews :: UpdateNewsRequest -> NewsRawPartial
requestToUpdateNews (UpdateNewsRequest nrt) = NewsRawPartial nrt

newsToResponse :: NewsNested -> CreateNewsResponse
newsToResponse = CreateNewsResponse
