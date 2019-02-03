{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Models.Tag where

import           Data.Text
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           WebServer.Database

data Tag = Tag {
  tagId :: Integer,
  tagName :: Text
}

instance Persistent Tag where
  tableName _ = "tags"

instance Fit TagRaw Tag where

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

data TagRaw = TagRaw {
  tagRawName :: Text
}

instance ToRow TagRaw where
  toRow TagRaw {..} =
    [ toField Default
    , toField tagRawName
    ]