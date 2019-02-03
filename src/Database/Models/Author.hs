{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.Author where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Data.Text
import           Database.Models.User
import           Database.Queries.Queries
import           WebServer.Database
import           WebServer.Pagination

data Author = Author {
  authorId :: Integer,
  authorUserId :: Integer,
  authorDescription :: Text
} deriving Show

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

instance Persistent (Author, User) where
  tableName = error "No table for (Author, User)"

  select :: Connection -> (Limit, Offset) -> IO [(Author, User)]
  select conn (Limit limit, Offset offset) =
    fmap inductiveTupleToTuple <$> (query conn authorsQuery [limit, offset])
   where
    authorsQuery
      = "SELECT  a.*, u.* FROM authors a \
          \INNER JOIN users u \
          \ON u.id = a.user_id \
          \LIMIT ? OFFSET ? ;"

  selectById conn id = undefined

instance Fit (AuthorRaw, UserRaw) (Author, User) where
  insert conn (AuthorRaw {..}, userRaw) =
    withTransaction conn $ insert conn userRaw >>= addAuthor
   where
    addAuthor (Just user@User {..}) = do
      authorList <- query conn dbQuery (userId, authorRawDescription)
      case authorList of
        [author] -> pure $ Just (author, user)
        _        -> pure Nothing
    addAuthor Nothing = pure Nothing
    dbQuery =
      "INSERT INTO authors \
      \VALUES (default,?,?) \
      \RETURNING * ;"

data AuthorNested = AuthorNested {
  authorNestedId :: Integer,
  authorNestedUser :: User,
  authorNestedDescription :: Text
}

data AuthorRaw = AuthorRaw {
  authorRawDescription :: Text
}
