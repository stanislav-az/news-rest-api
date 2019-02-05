{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Commentary where

import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Models.Commentary
import           WebServer.Database
import           Data.Maybe                     ( listToMaybe )

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
  :: Connection -> Integer -> CommentaryRaw -> IO (Maybe Commentary)
insertCommentary conn newsId CommentaryRaw {..} =
  selectById conn newsId
    >>= (maybe (pure Nothing) (insertIfPosted . newsIsDraft))
 where
  insertIfPosted isDraft = if isDraft
    then pure Nothing
    else listToMaybe <$> query conn insertQuery (commentaryRawContent, newsId)
  insertQuery
    = "INSERT INTO commentaries \
    \VALUES (default,?,?) \
    \RETURNING * ;"
