{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Models.User where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           Data.Text
import           Data.Time
import           WebServer.Database
import           WebServer.Pagination

data User = User {
  userId :: Integer,
  userName :: Text,
  userSurname :: Text,
  userAvatar :: Text,
  userDateCreated :: LocalTime,
  userIsAdmin :: Bool
} deriving Show

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance Persistent User where
  tableName _ = "users"

data UserRaw = UserRaw {
  userRawName :: Text,
  userRawSurname :: Text,
  userRawAvatar :: Text
}

instance ToRow UserRaw where
  toRow UserRaw {..} =
    [ toField Default
    , toField userRawName
    , toField userRawSurname
    , toField userRawAvatar
    , toField Default
    , toField Default
    ]
