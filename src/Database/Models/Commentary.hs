{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.Commentary where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           Data.Text
import           WebServer.Database

data Commentary = Commentary {
  commentaryId :: Integer,
  commentaryContent :: Text,
  commentaryNewsId :: Integer,
  commentaryUserId :: Integer
}

instance FromRow Commentary where
  fromRow = Commentary <$> field <*> field <*> field <*> field

instance Persistent Commentary where
  tableName _ = "commentaries"
  
  select = error "Use selectCommentariesByNewsId"

data CommentaryRaw = CommentaryRaw {
    commentaryRawContent :: Text,
    commentaryRawUserId :: Integer
}
