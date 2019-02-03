{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Tag where

import           Database.PostgreSQL.Simple
import           Database.Models.Tag

getTagsByNewsId :: Connection -> Integer -> IO [Tag]
getTagsByNewsId conn newsId = query conn q (Only newsId)
 where
  q
    = "SELECT t.id, t.name FROM tags t \
      \JOIN tags_news tn ON t.id = tn.tag_id \
      \JOIN news n ON tn.news_id = n.id \
      \WHERE n.id = ?"

updateTag :: Connection -> Integer -> TagRaw -> IO Tag
updateTag conn tagId TagRaw {..} =
  head <$> query conn updateTagQuery (tagRawName, tagId)

updateTagQuery :: Query
updateTagQuery =
  "UPDATE tags SET \
    \name = ?\
    \WHERE id = ? \
    \ RETURNING id, name"
