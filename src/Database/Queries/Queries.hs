{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Queries.Queries where

import qualified Data.ByteString as B (ByteString(..))
import qualified Data.String as S (IsString(..))
import qualified Data.Text as T (Text(..), intercalate, null)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , FromRow(..)
  , query_
  )
import Database.PostgreSQL.Simple ((:.)(..))
import qualified Database.PostgreSQL.Simple.Types as PSQL (Query(..))

getList :: PSQL.FromRow a => PSQL.Connection -> PSQL.Query -> IO [a]
getList conn tableName = PSQL.query_ conn $ "SELECT * FROM " <> tableName

inductiveTupleToTuple :: (a :. b) -> (a, b)
inductiveTupleToTuple (u :. a) = (u, a)

nameFieldOnSQL :: T.Text -> Maybe T.Text -> T.Text
nameFieldOnSQL fieldName = maybe "" nameField
  where
    nameField fieldValue = fieldName <> " = '" <> fieldValue <> "'"

makeQueryParameters :: [(T.Text, Maybe T.Text)] -> PSQL.Query
makeQueryParameters kvps =
  textToQuery . T.intercalate "," . filter (not . T.null) $
  map (uncurry nameFieldOnSQL) kvps

textToQuery :: T.Text -> PSQL.Query
textToQuery = PSQL.Query . T.encodeUtf8

showToQuery :: (Show a) => a -> PSQL.Query
showToQuery = S.fromString . show

bsToQuery :: B.ByteString -> PSQL.Query
bsToQuery = PSQL.Query
