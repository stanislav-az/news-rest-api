{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Database.Queries.Author where

import           Database.Models.Author
import           Database.Models.User
import           Control.Exception              ( bracket )
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple
import           Database.Connection
import           Database.Queries.Queries
import           Database.Queries.User
import qualified Data.Text                     as T

getAuthorsList :: IO [(User, Author)]
getAuthorsList = bracket (connect connectInfo) close $ \conn ->
  fmap inductiveTupleToTuple
    <$> (query_ conn authorsQuery :: IO [User :. Author])
 where
  authorsQuery
    = "SELECT  u.*, a.*  FROM authors AS a \
    \INNER JOIN users AS u \
    \ON u.user_id = a.user_id"

addAuthorToDB :: (UserRaw, AuthorRaw) -> IO (User, Author)
addAuthorToDB (UserRaw {..}, AuthorRaw {..}) =
  bracket (connect connectInfo) close $ \conn -> withTransaction conn $ do
    (user : _) <- query conn
                        insertUserQuery
                        (userRawName, userRawSurname, userRawAvatar)
    (author : _) <- query conn
                          insertAuthorQuery
                          (userId user, authorRawDescription)
    pure (user, author)

updateAuthor :: Integer -> AuthorRaw -> IO Author
updateAuthor authorId AuthorRaw {..} =
  bracket (connect connectInfo) close $ \conn ->
    head <$> query conn updateAuthorQuery (authorRawDescription, authorId)

insertAuthorQuery :: Query
insertAuthorQuery =
  "INSERT INTO authors(author_id, user_id, description) VALUES (default,?,?) \
  \RETURNING author_id, user_id, description"

updateAuthorQuery :: Query
updateAuthorQuery =
  "UPDATE authors SET \
    \description = ?\
    \WHERE author_id = ? \
    \ RETURNING author_id, user_id, description"
