{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.Tag where

import           Database.PostgreSQL.Simple
import           Database.Models.Tag
import           Database.Connection
import           Database.Queries.Queries
import qualified Data.Text                     as T

getTagsList :: Connection -> IO [Tag]
getTagsList conn = getList conn "tags"

getTagsByNewsId :: Connection -> Integer -> IO [Tag]
getTagsByNewsId conn newsId = query conn q (Only newsId)
 where
  q
    = "SELECT t.tag_id, t.name FROM tags t \
      \JOIN tags_news tn ON t.tag_id = tn.tag_id \
      \JOIN news n ON tn.news_id = n.news_id \
      \WHERE n.news_id = ?"

addTagToDB :: Connection -> TagRaw -> IO Tag
addTagToDB conn TagRaw {..} =
  head <$> query conn insertTagQuery (Only tagRawName)

updateTag :: Connection -> Integer -> TagRaw -> IO Tag
updateTag conn tagId TagRaw {..} =
  head <$> query conn updateTagQuery (tagRawName, tagId)

insertTagQuery :: Query
insertTagQuery =
  "INSERT INTO tags(tag_id, name) VALUES (default,?) \
  \ RETURNING tag_id, name"

updateTagQuery :: Query
updateTagQuery =
  "UPDATE tags SET \
    \name = ?\
    \WHERE tag_id = ? \
    \ RETURNING tag_id, name"
