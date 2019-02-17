{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.Commentary where

import qualified Database.PostgreSQL.Simple.FromRow
                                               as PSQL
                                                ( FromRow(..)
                                                , field
                                                )
import qualified Data.Text                     as T
                                                ( Text(..) )
import           WebServer.Database             ( Persistent(..) )

data Commentary = Commentary {
  commentaryId :: Integer,
  commentaryContent :: T.Text,
  commentaryNewsId :: Integer,
  commentaryUserId :: Integer
} deriving Show

instance PSQL.FromRow Commentary where
  fromRow =
    Commentary <$> PSQL.field <*> PSQL.field <*> PSQL.field <*> PSQL.field

instance Persistent Commentary where
  tableName _ = "commentaries"

  select = error "Use selectCommentariesByNewsId"

data CommentaryRaw = CommentaryRaw {
    commentaryRawContent :: T.Text,
    commentaryRawUserId :: Integer
}
