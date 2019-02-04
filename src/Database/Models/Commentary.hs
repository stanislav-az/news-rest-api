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
  commentaryNewsId :: Integer
}

instance FromRow Commentary where
  fromRow = Commentary <$> field <*> field <*> field

instance Persistent Commentary where
  tableName _ = "commentaries"

-- instance Fit CommentaryRaw Commentary where

data CommentaryRaw = CommentaryRaw {
    commentaryRawContent :: Text
  -- , commentaryRawNewsId :: Integer
}

-- instance ToRow CommentaryRaw where
--   toRow CommentaryRaw {..} =
--     [toField Default, toField commentaryRawContent, toField commentaryRawNewsId]
