{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.User where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           WebServer.Database             ( toPlaceHoldersList )
import           Database.Models.User
import           Database.Connection
import           Database.Queries.Queries
import qualified Data.Text                     as T

-- getUsersList :: Connection -> IO [User]
-- getUsersList conn = getList conn "users"

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING id, name, surname, avatar, date_created, is_admin"
