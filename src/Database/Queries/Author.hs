{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Database.Queries.Author where

import           Database.Models.Author
import           Database.Models.User
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple
import           Database.Connection
import           Database.Queries.Queries
import           Database.Queries.User
import qualified Data.Text                     as T

getAuthorNestedById :: Connection -> Integer -> IO AuthorNested
getAuthorNestedById conn authorId = do
  (Author {..} : _) <- query conn getAuthorQuery (Only authorId)
  (user        : _) <- query conn getUserQuery (Only authorUserId)
  pure $ AuthorNested { authorNestedId          = authorId
                      , authorNestedUser        = user
                      , authorNestedDescription = authorDescription
                      }
 where
  getAuthorQuery =
    "SELECT id, user_id, description FROM authors \
      \WHERE id = ?"
  getUserQuery
    = "SELECT id, name, surname, avatar, date_created, is_admin FROM users \
      \WHERE id = ?"

updateAuthor :: Connection -> Integer -> AuthorRaw -> IO Author
updateAuthor conn authorId AuthorRaw {..} =
  head <$> query conn updateAuthorQuery (authorRawDescription, authorId)

insertAuthorQuery :: Query
insertAuthorQuery =
  "INSERT INTO authors(id, user_id, description) VALUES (default,?,?) \
  \RETURNING id, user_id, description"

updateAuthorQuery :: Query
updateAuthorQuery =
  "UPDATE authors SET \
    \description = ?\
    \WHERE id = ? \
    \ RETURNING id, user_id, description"
