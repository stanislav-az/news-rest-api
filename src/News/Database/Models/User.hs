{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module News.Database.Models.User where

import qualified Data.Text as T (Text(..))
import qualified Data.Time as Time (LocalTime(..))
import qualified Database.PostgreSQL.Simple.FromRow as PSQL (FromRow(..), field)
import qualified Database.PostgreSQL.Simple.ToField as PSQL (toField)
import qualified Database.PostgreSQL.Simple.ToRow as PSQL (ToRow(..))
import qualified Database.PostgreSQL.Simple.Types as PSQL (Default(..))
import News.WebServer.Database (Fit(..), Persistent(..))

data User = User
  { userId :: Integer
  , userName :: T.Text
  , userSurname :: T.Text
  , userAvatar :: T.Text
  , userDateCreated :: Time.LocalTime
  , userIsAdmin :: Bool
  } deriving (Show)

data UserRaw = UserRaw
  { userRawName :: T.Text
  , userRawSurname :: T.Text
  , userRawAvatar :: T.Text
  }

instance PSQL.FromRow User where
  fromRow =
    User <$> PSQL.field <*> PSQL.field <*> PSQL.field <*> PSQL.field <*>
    PSQL.field <*>
    PSQL.field

instance Persistent User where
  tableName _ = "users"

instance Fit UserRaw User

instance PSQL.ToRow UserRaw where
  toRow UserRaw {..} =
    [ PSQL.toField PSQL.Default
    , PSQL.toField userRawName
    , PSQL.toField userRawSurname
    , PSQL.toField userRawAvatar
    , PSQL.toField PSQL.Default
    , PSQL.toField PSQL.Default
    ]
