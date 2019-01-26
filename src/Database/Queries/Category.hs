{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.Category where

import           Database.PostgreSQL.Simple
import           Database.Models.Category
import           Database.Connection
import           Database.Queries.Queries
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T
import           Data.String

getCategoriesList :: IO [CategoryNested]
getCategoriesList = getList "categories" >>= (mapM nestCategory)

addCategoryToDB :: CategoryRaw -> IO CategoryNested
addCategoryToDB cr =
  ( bracket (connect connectInfo) close
    $ \conn -> head <$> query_ conn (insertCategoryQuery cr)
    )
    >>= nestCategory

updateCategory :: T.Text -> CategoryRawPartial -> IO CategoryNested
updateCategory categoryId category =
  (bracket (connect connectInfo) close $ \conn ->
      head <$> query conn (updateCategoryQuery category) (Only categoryId)
    )
    >>= nestCategory

nestCategory :: Category -> IO CategoryNested
nestCategory (Category id name Nothing        ) = pure $ Parent id name
nestCategory (Category id name (Just parentId)) = do
  (parent : _) <- bracket (connect connectInfo) close
    $ \conn -> query conn selectParentQuery (Only parentId)
  parentNested <- nestCategory parent
  pure $ CategoryNested id name parentNested
 where
  selectParentQuery
    = "SELECT category_id, name, parent_id FROM categories \
      \WHERE category_id = ?"


insertCategoryQuery :: CategoryRaw -> Query
insertCategoryQuery CategoryRaw {..} =
  "INSERT INTO categories(category_id,name,parent_id) \
  \ VALUES (default, '"
    <> toQuery categoryRawName
    <> "', "
    <> maybeAddParent
    <> ") "
    <> "RETURNING category_id, name, parent_id"
 where
  maybeAddParent = maybe "default" (fromString . show) categoryRawParentId

updateCategoryQuery :: CategoryRawPartial -> Query
updateCategoryQuery CategoryRawPartial {..} =
  let params = makeQueryParameters
        [ ("name"     , categoryRawPartialName)
        , ("parent_id", T.pack . show <$> categoryRawPartialParentId)
        ]
  in  "UPDATE categories SET "
        <> params
        <> "WHERE category_id = ? "
        <> "RETURNING category_id, name, parent_id"
