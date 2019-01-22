{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.User where

import           Database.PostgreSQL.Simple
import           Database.Models.User
import Database.Connection
import           Database.Queries.Queries
import           Control.Exception (bracket)
import qualified Data.Text                     as T
import           Data.String

getUsersList :: IO [User]
getUsersList = getList "users"

addUserToDB :: UserRaw -> IO User
addUserToDB UserRaw {..} = bracket (connect connectInfo) close $ \conn ->
  head <$> query conn
                 insertUserQuery
                 (userRawName, userRawSurname, userRawAvatar)

updateUser :: T.Text -> UserRawPartial -> IO User
updateUser uid user = bracket (connect connectInfo) close $ \conn -> do
  let qText = (updateUserQuery uid user)
  print qText
  res <- query conn qText ()
  pure $ head res

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING user_id, name, surname, avatar, date_created, is_admin"


updateUserQuery :: T.Text -> UserRawPartial -> Query
updateUserQuery uid UserRawPartial {..} =
  let
    toQuery  = fromString . T.unpack
    nameExpr = maybe "" (\name -> "name = '" <> name <> "'") userRawPartialName
    surnameExpr =
      maybe "" (((<>) "'") . ((<>) "surname= '")) userRawPartialSurname
    avatarExpr =
      maybe "" (((<>) "'") . ((<>) "avatar= '")) userRawPartialAvatar
    params =
      toQuery
        . T.intercalate ","
        . filter (not . T.null)
        $ [nameExpr, surnameExpr, avatarExpr]
  in
    "UPDATE users SET "
    <> params
    <> "WHERE user_id="
    <> toQuery uid
    <> "RETURNING user_id, name, surname, avatar, date_created, is_admin"
