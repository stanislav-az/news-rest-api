{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Category where

import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple
import           Database.Models.Category
import           Database.Queries.Queries

updateCategory
  :: Connection -> Integer -> CategoryRawPartial -> IO CategoryNested
updateCategory conn categoryId category =
  (head <$> query conn (updateCategoryQuery category) (Only categoryId))
    >>= (nestCategory conn)

updateCategoryQuery :: CategoryRawPartial -> Query
updateCategoryQuery CategoryRawPartial {..} =
  let params = makeQueryParameters
        [ ("name"     , categoryRawPartialName)
        , ("parent_id", T.pack . show <$> categoryRawPartialParentId)
        ]
  in  "UPDATE categories SET "
        <> params
        <> "WHERE id = ? "
        <> "RETURNING id, name, parent_id"
