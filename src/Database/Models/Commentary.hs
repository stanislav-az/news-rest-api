{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.Commentary where

import qualified Data.Text as T (Text(..))
import qualified Database.PostgreSQL.Simple.FromRow as PSQL (FromRow(..), field)
import WebServer.Database (Persistent(..))

data Commentary = Commentary
  { commentaryId :: Integer
  , commentaryContent :: T.Text
  , commentaryNewsId :: Integer
  , commentaryUserId :: Integer
  } deriving (Show)

data CommentaryRaw = CommentaryRaw
  { commentaryRawContent :: T.Text
  , commentaryRawUserId :: Integer
  }

instance PSQL.FromRow Commentary where
  fromRow =
    Commentary <$> PSQL.field <*> PSQL.field <*> PSQL.field <*> PSQL.field

instance Persistent Commentary where
  tableName _ = "commentaries"
  select = error "Use selectCommentariesByNewsId"
