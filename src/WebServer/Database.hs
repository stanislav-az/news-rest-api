{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

module WebServer.Database where

import           Database.PostgreSQL.Simple
import           WebServer.Pagination
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
  -- insert :: Connection -> IO entity
  -- update :: Connection -> Integer -> IO entity
  -- delete :: Connection -> Integer -> IO ()
