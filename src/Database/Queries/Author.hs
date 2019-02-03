{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Author where

import           Database.PostgreSQL.Simple
import           Database.Models.Author

updateAuthor :: Connection -> Integer -> AuthorRaw -> IO Author
updateAuthor conn authorId AuthorRaw {..} =
  head <$> query conn updateAuthorQuery (authorRawDescription, authorId)

updateAuthorQuery :: Query
updateAuthorQuery =
  "UPDATE authors SET \
    \description = ?\
    \WHERE id = ? \
    \ RETURNING id, user_id, description"
