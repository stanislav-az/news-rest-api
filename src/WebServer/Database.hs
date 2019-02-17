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

import qualified Database.PostgreSQL.Simple    as PSQL
                                                ( Connection(..)
                                                , Query(..)
                                                , FromRow(..)
                                                , execute
                                                , query
                                                )
import qualified Database.PostgreSQL.Simple.ToRow
                                               as PSQL
                                                ( ToRow(..)
                                                , toRow
                                                )
import qualified Data.Proxy                    as P
                                                ( Proxy(..) )
import qualified Data.Maybe                    as MB
                                                ( listToMaybe )
import           WebServer.UrlParser.Pagination ( Limit(..)
                                                , Offset(..)
                                                )

class Persistent entity where
  tableName :: P.Proxy entity -> PSQL.Query

  select :: PSQL.Connection -> (Limit, Offset) -> IO [entity]
  default select :: (PSQL.FromRow entity) => PSQL.Connection -> (Limit, Offset) -> IO [entity]
  select conn (Limit limit, Offset offset) = PSQL.query conn dbQuery [limit, offset]
    where
      dbQuery = "SELECT * FROM " <> tableName (P.Proxy :: P.Proxy entity) <> " LIMIT ? OFFSET ? ;"

  selectById :: PSQL.Connection -> Integer -> IO (Maybe entity)
  default selectById :: (PSQL.FromRow entity) => PSQL.Connection -> Integer -> IO (Maybe entity)
  selectById conn id = MB.listToMaybe <$> PSQL.query conn dbQuery [id]
    where
      dbQuery = "SELECT * FROM " <> tableName (P.Proxy :: P.Proxy entity) <> " WHERE id = ? ;"

  delete :: P.Proxy entity -> PSQL.Connection -> Integer -> IO Bool
  delete _ conn id = checkAffectedRows <$> PSQL.execute conn deleteQuery [id]
      where
        thisTable = tableName (P.Proxy :: P.Proxy entity)
        checkAffectedRows 0 = False
        checkAffectedRows _ = True
        deleteQuery = "DELETE FROM " <> thisTable <> " WHERE id = ? ;"

class (Persistent stored) => Fit object stored where
  insert :: PSQL.Connection -> object -> IO (Maybe stored)
  default insert :: (PSQL.FromRow stored, PSQL.ToRow object) => PSQL.Connection -> object -> IO (Maybe stored)
  insert conn item = MB.listToMaybe <$> PSQL.query conn dbQuery item
    where
      placeholders = toPlaceHoldersList item
      dbQuery = "INSERT INTO " <> tableName (P.Proxy :: P.Proxy stored) <> " VALUES " <> placeholders <> " RETURNING * ;"

toPlaceHoldersList :: (PSQL.ToRow a) => a -> PSQL.Query
toPlaceHoldersList = replicateWithPositiveLength . length . PSQL.toRow
 where
  replicateWithPositiveLength 0 = ""
  replicateWithPositiveLength 1 = "(?)"
  replicateWithPositiveLength l =
    "(" <> (mconcat $ replicate (l - 1) "?,") <> "?)"
