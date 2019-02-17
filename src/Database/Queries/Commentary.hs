{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Commentary where

import qualified Database.PostgreSQL.Simple    as PSQL
                                                ( Connection(..)
                                                , query
                                                )
import           Database.Models.News           ( News(..) )
import           Database.Models.Commentary     ( Commentary(..)
                                                , CommentaryRaw(..)
                                                )
import           Database.Models.User           ( User(..) )
import           WebServer.Database             ( Limit(..)
                                                , Offset(..)
                                                , selectById
                                                )
import           Helpers                        ( listToEither )

selectCommentariesByNewsId
  :: PSQL.Connection -> (Limit, Offset) -> Integer -> IO [Commentary]
selectCommentariesByNewsId conn (Limit limit, Offset offset) newsId =
  PSQL.query conn dbQuery [newsId, limit, offset]
 where
  dbQuery
    = "SELECT  * FROM commentaries \
      \WHERE news_id = ? \
      \LIMIT ? OFFSET ? ;"

insertCommentary
  :: PSQL.Connection
  -> Integer
  -> CommentaryRaw
  -> IO (Either String Commentary)
insertCommentary conn newsId CommentaryRaw {..} =
  selectById conn newsId
    >>= (maybe (pure $ Left "Failed to find news by id on commentary insert")
               (insertIfPosted . newsIsDraft)
        )
 where
  insertIfPosted isDraft = if isDraft
    then pure $ Left "Tried to insert commentary to a draft"
    else listToEither "Failed to insert commentary" <$> PSQL.query
      conn
      insertQuery
      (commentaryRawContent, newsId, commentaryRawUserId)
  insertQuery
    = "INSERT INTO commentaries \
    \VALUES (default,?,?,?) \
    \RETURNING * ;"

isAuthorOfCommentary :: PSQL.Connection -> User -> Integer -> IO Bool
isAuthorOfCommentary conn user commentaryId = do
  mbCommentary <- selectById conn commentaryId
  pure $ maybe False (\c -> commentaryUserId c == userId user) mbCommentary
