{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Tag where

import Database.Models.Tag (Tag(..), TagRaw(..))
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , Query(..)
  , query
  )

getTagsByNewsId :: PSQL.Connection -> Integer -> IO [Tag]
getTagsByNewsId conn newsId = PSQL.query conn dbQuery [newsId]
  where
    dbQuery =
      "SELECT t.id, t.name FROM tags t \
      \JOIN tags_news tn ON t.id = tn.tag_id \
      \JOIN news n ON tn.news_id = n.id \
      \WHERE n.id = ?"

updateTag :: PSQL.Connection -> Integer -> TagRaw -> IO Tag
updateTag conn tagId TagRaw {..} =
  head <$> PSQL.query conn updateTagQuery (tagRawName, tagId)

updateTagQuery :: PSQL.Query
updateTagQuery =
  "UPDATE tags SET \
    \name = ?\
    \WHERE id = ? \
    \ RETURNING id, name"
