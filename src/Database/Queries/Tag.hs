{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.Tag where

import           Database.PostgreSQL.Simple
import           Database.Models.Tag
import           Database.Connection
import           Database.Queries.Queries
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T

getTagsList :: IO [Tag]
getTagsList = getList "tags"

addTagToDB :: TagRaw -> IO Tag
addTagToDB TagRaw {..} = bracket (connect connectInfo) close
  $ \conn -> head <$> query conn insertTagQuery (Only tagRawName)

updateTag :: T.Text -> TagRaw -> IO Tag
updateTag tagId TagRaw {..} = bracket (connect connectInfo) close
  $ \conn -> head <$> query conn updateTagQuery (tagRawName, tagId)

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
