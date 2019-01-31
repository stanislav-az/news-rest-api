{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.User where

import           Database.PostgreSQL.Simple
import           Database.Models.User
import           Database.Connection
import           Database.Queries.Queries
import qualified Data.Text                     as T

getUsersList :: Connection -> IO [User]
getUsersList conn = getList conn "users"

getUserById :: Connection -> Integer -> IO (Maybe User)
getUserById conn userId = do
    user <- query conn q [userId]
    case user of
      []         -> pure Nothing
      (user : _) -> pure (Just user)
    where
      q = "SELECT * FROM users WHERE user_id=?"

addUserToDB :: Connection -> UserRaw -> IO User
addUserToDB conn UserRaw {..} = head <$> query conn
                        insertUserQuery
                        (userRawName, userRawSurname, userRawAvatar)

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING user_id, name, surname, avatar, date_created, is_admin"
