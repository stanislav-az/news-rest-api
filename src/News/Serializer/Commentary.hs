{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module News.Serializer.Commentary where

import qualified Data.Aeson as JSON
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  )
import Data.Aeson ((.:), (.=))
import News.Database.Models.Commentary (Commentary(..), CommentaryRaw(..))

newtype CreateCommentaryRequest =
  CreateCommentaryRequest CommentaryRaw

newtype CreateCommentaryResponse =
  CreateCommentaryResponse Commentary

instance JSON.FromJSON CreateCommentaryRequest where
  parseJSON =
    JSON.withObject "CreateCommentaryRequest" $ \v ->
      fmap CreateCommentaryRequest $
      CommentaryRaw <$> v .: "content" <*> v .: "user_id"

instance JSON.ToJSON CreateCommentaryResponse where
  toJSON (CreateCommentaryResponse Commentary {..}) =
    JSON.object
      [ "id" .= commentaryId
      , "content" .= commentaryContent
      , "news_id" .= commentaryNewsId
      , "user_id" .= commentaryUserId
      ]

requestToCommentary :: CreateCommentaryRequest -> CommentaryRaw
requestToCommentary (CreateCommentaryRequest commentary) = commentary

commentaryToResponse :: Commentary -> CreateCommentaryResponse
commentaryToResponse = CreateCommentaryResponse
