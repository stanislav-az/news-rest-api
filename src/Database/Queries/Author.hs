{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Author where

import Database.Models.Author (Author(..), AuthorRaw(..))
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , Query(..)
  , query
  )

updateAuthor :: PSQL.Connection -> Integer -> AuthorRaw -> IO Author
updateAuthor conn authorId AuthorRaw {..} =
  head <$> PSQL.query conn updateAuthorQuery (authorRawDescription, authorId)

updateAuthorQuery :: PSQL.Query
updateAuthorQuery =
  "UPDATE authors SET \
    \description = ?\
    \WHERE id = ? \
    \ RETURNING id, user_id, description"
