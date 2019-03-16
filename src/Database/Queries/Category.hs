{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Category where

import Database.Models.Category
  ( CategoryNested(..)
  , CategoryRawPartial(..)
  , nestCategory
  )
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , Query(..)
  , query
  )
import Database.Queries.Queries (makeQueryParameters)
import Helpers (texify)

updateCategory ::
     PSQL.Connection -> Integer -> CategoryRawPartial -> IO CategoryNested
updateCategory conn categoryId category =
  (head <$> PSQL.query conn (updateCategoryQuery category) [categoryId]) >>=
  (nestCategory conn)

updateCategoryQuery :: CategoryRawPartial -> PSQL.Query
updateCategoryQuery CategoryRawPartial {..} =
  let params =
        makeQueryParameters
          [ ("name", categoryRawPartialName)
          , ("parent_id", texify <$> categoryRawPartialParentId)
          ]
   in "UPDATE categories SET " <> params <> "WHERE id = ? " <>
      "RETURNING id, name, parent_id"
