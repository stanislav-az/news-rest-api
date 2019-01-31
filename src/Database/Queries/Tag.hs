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
