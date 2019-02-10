{-# LANGUAGE OverloadedStrings #-}

module Database.Queries.Queries where

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString               as B
import           Data.String
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

getList :: FromRow a => Connection -> Query -> IO [a]
getList conn tableName = query_ conn $ "SELECT * FROM " <> tableName

inductiveTupleToTuple (u :. a) = (u, a)

nameFieldOnSQL :: T.Text -> Maybe T.Text -> T.Text
nameFieldOnSQL fieldName = maybe "" nameField
  where nameField fieldValue = fieldName <> " = '" <> fieldValue <> "'"

makeQueryParameters :: [(T.Text, Maybe T.Text)] -> Query
makeQueryParameters kvps =
  textToQuery . T.intercalate "," . filter (not . T.null) $ map
    (uncurry nameFieldOnSQL)
    kvps

textToQuery :: T.Text -> Query
textToQuery = Query . T.encodeUtf8

showToQuery :: (Show a) => a -> Query
showToQuery = fromString . show

bsToQuery :: B.ByteString -> Query
bsToQuery = Query