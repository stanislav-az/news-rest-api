{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.Author where

import qualified Data.Text as T (Text(..))
import Database.Models.User (User(..), UserRaw(..))
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , query
  , withTransaction
  )
import qualified Database.PostgreSQL.Simple.FromRow as PSQL (FromRow(..), field)
import Database.Queries.Queries (inductiveTupleToTuple)
import WebServer.Database (Fit(..), Limit(..), Offset(..), Persistent(..))

data Author = Author
  { authorId :: Integer
  , authorUserId :: Integer
  , authorDescription :: T.Text
  } deriving (Show)

data AuthorRaw = AuthorRaw
  { authorRawDescription :: T.Text
  }

instance PSQL.FromRow Author where
  fromRow = Author <$> PSQL.field <*> PSQL.field <*> PSQL.field

instance Persistent Author where
  tableName _ = "authors"

instance Persistent (Author, User) where
  tableName = error "No table for (Author, User)"
  select :: PSQL.Connection -> (Limit, Offset) -> IO [(Author, User)]
  select conn (Limit limit, Offset offset) =
    fmap inductiveTupleToTuple <$>
    (PSQL.query conn authorsQuery [limit, offset])
    where
      authorsQuery =
        "SELECT  a.*, u.* FROM authors a \
          \INNER JOIN users u \
          \ON u.id = a.user_id \
          \LIMIT ? OFFSET ? ;"
  selectById conn authorId =
    selectById conn authorId >>= maybe (pure Nothing) selectUser
    where
      selectUser a@Author {..} =
        maybe Nothing (\u -> Just (a, u)) <$> selectById conn authorUserId

instance Fit (AuthorRaw, UserRaw) (Author, User) where
  insert conn (AuthorRaw {..}, userRaw) =
    PSQL.withTransaction conn $ insert conn userRaw >>= insertAuthor
    where
      insertAuthor (Just user@User {..}) = do
        authorList <- PSQL.query conn dbQuery (userId, authorRawDescription)
        case authorList of
          [author] -> pure $ Just (author, user)
          _ -> pure Nothing
      insertAuthor Nothing = pure Nothing
      dbQuery =
        "INSERT INTO authors \
      \VALUES (default,?,?) \
      \RETURNING * ;"
