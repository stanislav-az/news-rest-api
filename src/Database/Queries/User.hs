{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.User where

import           Database.PostgreSQL.Simple
import           Database.Models.User
import           Database.Connection
import           Database.Queries.Queries
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T

getUsersList :: IO [User]
getUsersList = getList "users"

getUserById :: Integer -> IO (Maybe User)
getUserById userId = bracket (connect connectInfo) close $ \conn -> do
  user <- query conn q [userId]
  case user of
    []         -> pure Nothing
    (user : _) -> pure (Just user)
  where q = "SELECT * FROM users WHERE user_id=?"

addUserToDB :: UserRaw -> IO User
addUserToDB UserRaw {..} = bracket (connect connectInfo) close $ \conn ->
  head <$> query conn
                 insertUserQuery
                 (userRawName, userRawSurname, userRawAvatar)

updateUser :: Integer -> UserRawPartial -> IO User
updateUser userId user = bracket (connect connectInfo) close
  $ \conn -> head <$> query conn (updateUserQuery user) (Only userId)

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING user_id, name, surname, avatar, date_created, is_admin"

updateUserQuery :: UserRawPartial -> Query
updateUserQuery UserRawPartial {..} =
  let params = makeQueryParameters
        [ ("name"   , userRawPartialName)
        , ("surname", userRawPartialSurname)
        , ("avatar" , userRawPartialAvatar)
        ]
  in  "UPDATE users SET "
        <> params
        <> "WHERE user_id = ? "
        <> "RETURNING user_id, name, surname, avatar, date_created, is_admin"
