{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module News.Database.Models.Tag where

import qualified Data.Text as T (Text(..))
import qualified Database.PostgreSQL.Simple.FromRow as PSQL (FromRow(..), field)
import qualified Database.PostgreSQL.Simple.ToField as PSQL (toField)
import qualified Database.PostgreSQL.Simple.ToRow as PSQL (ToRow(..))
import qualified Database.PostgreSQL.Simple.Types as PSQL (Default(..))
import News.WebServer.Database (Fit(..), Persistent(..))

data Tag = Tag
  { tagId :: Integer
  , tagName :: T.Text
  } deriving (Show)

data TagRaw = TagRaw
  { tagRawName :: T.Text
  }

instance Persistent Tag where
  tableName _ = "tags"

instance Fit TagRaw Tag

instance PSQL.FromRow Tag where
  fromRow = Tag <$> PSQL.field <*> PSQL.field

instance PSQL.ToRow TagRaw where
  toRow TagRaw {..} = [PSQL.toField PSQL.Default, PSQL.toField tagRawName]
