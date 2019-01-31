{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.Category where

import           Database.PostgreSQL.Simple
import           Database.Models.Category
import           Database.Connection
import           Database.Queries.Queries
import qualified Data.Text                     as T
import           Data.String

getCategoriesList :: Connection -> IO [CategoryNested]
getCategoriesList conn =
  getList conn "categories" >>= (mapM (nestCategory conn))

getCategoryById :: Connection -> Integer -> IO CategoryNested
getCategoryById conn categoryId =
  (head <$> query conn selectQueryByIdQuery (Only categoryId))
    >>= (nestCategory conn)

addCategoryToDB :: Connection -> CategoryRaw -> IO CategoryNested
addCategoryToDB conn cr =
  (head <$> query_ conn (insertCategoryQuery cr)) >>= (nestCategory conn)

updateCategory
  :: Connection -> Integer -> CategoryRawPartial -> IO CategoryNested
updateCategory conn categoryId category =
  (head <$> query conn (updateCategoryQuery category) (Only categoryId))
    >>= (nestCategory conn)

nestCategory :: Connection -> Category -> IO CategoryNested
nestCategory conn (Category id name Nothing        ) = pure $ Parent id name
nestCategory conn (Category id name (Just parentId)) = do
  (parent : _) <- query conn selectQueryByIdQuery (Only parentId)
  parentNested <- nestCategory conn parent
  pure $ CategoryNested id name parentNested

selectQueryByIdQuery :: Query
selectQueryByIdQuery =
  "SELECT category_id, name, parent_id FROM categories \
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
