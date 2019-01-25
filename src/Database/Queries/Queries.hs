{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Queries where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple
import           Control.Exception              ( bracket )
import           Database.Connection
import qualified Data.Text                     as T
import           Data.String

getList :: FromRow a => Query -> IO [a]
getList tableName = bracket (connect connectInfo) close
  $ \conn -> query_ conn $ "SELECT * FROM " <> tableName

inductiveTupleToTuple (u :. a) = (u, a)

nameFieldOnSQL :: T.Text -> Maybe T.Text -> T.Text
nameFieldOnSQL fieldName = maybe "" nameField
  where nameField fieldValue = fieldName <> " = '" <> fieldValue <> "'"

makeQueryParameters :: [(T.Text, Maybe T.Text)] -> Query
makeQueryParameters kvps =
  toQuery . T.intercalate "," . filter (not . T.null) $ map
    (uncurry nameFieldOnSQL)
    kvps

toQuery = fromString . T.unpack
