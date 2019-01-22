{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Queries where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple
import           Control.Exception              ( bracket )
import           Database.Connection
getList :: FromRow a => Query -> IO [a]
getList tableName = bracket (connect connectInfo) close
  $ \conn -> query_ conn $ "SELECT * FROM " <> tableName

inductiveTupleToTuple (u :. a) = (u, a)
