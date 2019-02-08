{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Commentary where

import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Models.Commentary
import           WebServer.Database
import           Helpers

selectCommentariesByNewsId
  :: Connection -> (Limit, Offset) -> Integer -> IO [Commentary]
selectCommentariesByNewsId conn (Limit limit, Offset offset) newsId = query
  conn
  dbQuery
  [newsId, limit, offset]
 where
  dbQuery
    = "SELECT  * FROM commentaries \
      \WHERE news_id = ? \
      \LIMIT ? OFFSET ? ;"

insertCommentary
  :: Connection -> Integer -> CommentaryRaw -> IO (Either String Commentary)
insertCommentary conn newsId CommentaryRaw {..} =
  selectById conn newsId
    >>= (maybe (pure $ Left "Failed to find news by id on commentary insert")
               (insertIfPosted . newsIsDraft)
        )
 where
  insertIfPosted isDraft = if isDraft
    then pure $ Left "Tried to insert commentary to a draft"
    else listToEither "Failed to insert commentary"
      <$> query conn insertQuery (commentaryRawContent, newsId)
  insertQuery =
    "INSERT INTO commentaries \
    \VALUES (default,?,?) \
    \RETURNING * ;"
