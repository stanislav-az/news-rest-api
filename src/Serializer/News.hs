{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer.News where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Database.Models.News
import           Data.Functor.Identity

newtype CreateNewsRequest = CreateNewsRequest (NewsRawT Identity)

instance FromJSON CreateNewsRequest where
  parseJSON = withObject "CreateNewsRequest" $ \v ->
    fmap CreateNewsRequest
      $   NewsRawT
      <$> (Identity <$> (v .: "title"))
      <*> (Identity <$> (v .: "user_id"))
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
      .:? "user_id"
      <*> v
      .:? "category_id"
      <*> v
      .:? "content"
      <*> v
      .:? "main_photo"
      <*> v
      .:? "tags"

newtype CreateNewsResponse = CreateNewsResponse News

instance ToJSON CreateNewsResponse where
  toJSON (CreateNewsResponse News {..}) = object
    [ "news_id" .= newsId
    , "title" .= newsTitle
    , "date_created" .= newsDateCreated
    , "user_id" .= newsUserId
    , "category_id" .= newsCategoryId
    , "content" .= newsContent
    , "main_photo" .= newsMainPhoto
    , "is_draft" .= newsIsDraft
    ]

requestToNews :: CreateNewsRequest -> NewsRaw
requestToNews (CreateNewsRequest nrt) = NewsRaw nrt

requestToUpdateNews :: UpdateNewsRequest -> NewsRawPartial
requestToUpdateNews (UpdateNewsRequest nrt) = NewsRawPartial nrt

newsToResponse :: News -> CreateNewsResponse
newsToResponse = CreateNewsResponse
