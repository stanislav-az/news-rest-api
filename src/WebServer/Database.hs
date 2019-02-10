{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WebServer.Database
  ( Limit(..)
  , Offset(..)
  , Persistent(..)
  , Fit(..)
  )
where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           WebServer.UrlParser.Pagination
import           Data.Proxy
import           Data.Maybe                     ( listToMaybe )

class Persistent entity where
  tableName :: Proxy entity -> Query

  select :: Connection -> (Limit, Offset) -> IO [entity]
  default select :: (FromRow entity) => Connection -> (Limit, Offset) -> IO [entity]
  select conn (Limit limit, Offset offset) = query conn dbQuery [limit, offset]
    where
      dbQuery = "SELECT * FROM " <> tableName (Proxy :: Proxy entity) <> " LIMIT ? OFFSET ? ;"

  selectById :: Connection -> Integer -> IO (Maybe entity)
  default selectById :: (FromRow entity) => Connection -> Integer -> IO (Maybe entity)
  selectById conn id = listToMaybe <$> query conn dbQuery [id]
    where
      dbQuery = "SELECT * FROM " <> tableName (Proxy :: Proxy entity) <> " WHERE id = ? ;"

  delete :: Proxy entity -> Connection -> Integer -> IO Bool
  delete _ conn id = checkAffectedRows <$> execute conn deleteQuery [id]
      where
        thisTable = tableName (Proxy :: Proxy entity)
        checkAffectedRows 0 = False
        checkAffectedRows _ = True
        deleteQuery = "DELETE FROM " <> thisTable <> " WHERE id = ? ;"

class (Persistent stored) => Fit object stored where
  insert :: Connection -> object -> IO (Maybe stored)
  default insert :: (FromRow stored, ToRow object) => Connection -> object -> IO (Maybe stored)
  insert conn item = listToMaybe <$> query conn dbQuery item
    where
      placeholders = toPlaceHoldersList item
      dbQuery = "INSERT INTO " <> tableName (Proxy :: Proxy stored) <> " VALUES " <> placeholders <> " RETURNING * ;"

toPlaceHoldersList :: (ToRow a) => a -> Query
toPlaceHoldersList = replicateWithPositiveLength . length . toRow
 where
  replicateWithPositiveLength 0 = ""
  replicateWithPositiveLength 1 = "(?)"
  replicateWithPositiveLength l =
    "(" <> (mconcat $ replicate (l - 1) "?,") <> "?)"
