{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.User where

import           Data.Text
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           WebServer.Database

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

instance Fit UserRaw User where

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
