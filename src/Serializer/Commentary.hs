{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Commentary where

import           Data.Aeson
import           Database.Models.Commentary

newtype CreateCommentaryRequest = CreateCommentaryRequest CommentaryRaw

instance FromJSON CreateCommentaryRequest where
  parseJSON = withObject "CreateCommentaryRequest"
    $ \v -> fmap CreateCommentaryRequest $ CommentaryRaw <$> v .: "content"
      -- <*> v
      -- .:  "news_id"

newtype CreateCommentaryResponse = CreateCommentaryResponse Commentary

instance ToJSON CreateCommentaryResponse where
  toJSON (CreateCommentaryResponse Commentary {..}) = object
    [ "id" .= commentaryId
    , "content" .= commentaryContent
    , "news_id" .= commentaryNewsId
    ]

requestToCommentary :: CreateCommentaryRequest -> CommentaryRaw
requestToCommentary (CreateCommentaryRequest commentary) = commentary

commentaryToResponse :: Commentary -> CreateCommentaryResponse
commentaryToResponse = CreateCommentaryResponse
