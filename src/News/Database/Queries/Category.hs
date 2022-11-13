{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module News.Database.Queries.Category where

import News.Database.Models.Category
  ( CategoryNested(..)
  , CategoryRawPartial(..)
  , nestCategory
  )
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , Query(..)
  , query
  )
import News.Database.Queries.Queries (makeQueryParameters)
import Ext.Data.Text (textify)

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
          , ("parent_id", textify <$> categoryRawPartialParentId)
          ]
   in "UPDATE categories SET " <> params <> "WHERE id = ? " <>
      "RETURNING id, name, parent_id"
