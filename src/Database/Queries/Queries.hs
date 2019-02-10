{-# LANGUAGE OverloadedStrings #-}

module Database.Queries.Queries where

import qualified Data.Text                     as T
import           Data.String
import           Database.PostgreSQL.Simple

getList :: FromRow a => Connection -> Query -> IO [a]
getList conn tableName = query_ conn $ "SELECT * FROM " <> tableName

inductiveTupleToTuple (u :. a) = (u, a)

nameFieldOnSQL :: T.Text -> Maybe T.Text -> T.Text
nameFieldOnSQL fieldName = maybe "" nameField
  where nameField fieldValue = fieldName <> " = '" <> fieldValue <> "'"

makeQueryParameters :: [(T.Text, Maybe T.Text)] -> Query
makeQueryParameters kvps =
  toQuery . T.intercalate "," . filter (not . T.null) $ map
    (uncurry nameFieldOnSQL)
    kvps

toQuery :: T.Text -> Query
toQuery = fromString . T.unpack

showQuery :: (Show a) => a -> Query
showQuery = fromString . show
