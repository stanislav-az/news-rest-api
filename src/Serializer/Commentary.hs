{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Commentary where

import qualified Data.Aeson                    as JSON
                                                ( FromJSON(..)
                                                , ToJSON(..)
                                                , withObject
                                                , object
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                )
import           Database.Models.Commentary     ( Commentary(..)
                                                , CommentaryRaw(..)
                                                )

newtype CreateCommentaryRequest = CreateCommentaryRequest CommentaryRaw

instance JSON.FromJSON CreateCommentaryRequest where
  parseJSON = JSON.withObject "CreateCommentaryRequest" $ \v ->
    fmap CreateCommentaryRequest
      $   CommentaryRaw
      <$> v
      .:  "content"
      <*> v
      .:  "user_id"

newtype CreateCommentaryResponse = CreateCommentaryResponse Commentary

instance JSON.ToJSON CreateCommentaryResponse where
  toJSON (CreateCommentaryResponse Commentary {..}) = JSON.object
    [ "id" .= commentaryId
    , "content" .= commentaryContent
    , "news_id" .= commentaryNewsId
    , "user_id" .= commentaryUserId
    ]

requestToCommentary :: CreateCommentaryRequest -> CommentaryRaw
requestToCommentary (CreateCommentaryRequest commentary) = commentary

commentaryToResponse :: Commentary -> CreateCommentaryResponse
commentaryToResponse = CreateCommentaryResponse
