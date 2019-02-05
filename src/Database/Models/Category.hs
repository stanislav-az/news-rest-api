{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.Category where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Data.Text
import           WebServer.Database
import           Database.Queries.Queries
import           Data.Maybe                     ( listToMaybe )
import           Data.String

data Category = Category {
  categoryId :: Integer,
  categoryName :: Text,
  categoryParentId :: Maybe Integer
} deriving Show

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field

instance Persistent Category where
  tableName _ = "categories"

data CategoryNested = CategoryNested Integer Text CategoryNested | Parent Integer Text

instance Persistent CategoryNested where
  tableName _ = error "No table for CategoryNested"

  select conn pagination =
    select conn pagination >>= (mapM (nestCategory conn))

  selectById conn categoryId =
    selectById conn categoryId >>= (maybeNestCategory conn)

instance Fit CategoryRaw CategoryNested where
  insert conn cr =
    (listToMaybe <$> query_ conn (insertCategoryQuery cr))
      >>= (maybeNestCategory conn)

data CategoryRaw = CategoryRaw {
  categoryRawName :: Text,
  categoryRawParentId :: Maybe Integer
}

data CategoryRawPartial = CategoryRawPartial {
  categoryRawPartialName :: Maybe Text,
  categoryRawPartialParentId :: Maybe Integer
}

maybeNestCategory :: Connection -> Maybe Category -> IO (Maybe CategoryNested)
maybeNestCategory conn = maybe (pure Nothing) (fmap Just . nestCategory conn)

nestCategory :: Connection -> Category -> IO CategoryNested
nestCategory conn (Category id name Nothing        ) = pure $ Parent id name
nestCategory conn (Category id name (Just parentId)) = do
  (Just parent) <- selectById conn parentId -- Pattern matching to Just because db constraints let us to do it
  parentNested  <- nestCategory conn parent
  pure $ CategoryNested id name parentNested

insertCategoryQuery :: CategoryRaw -> Query
insertCategoryQuery CategoryRaw {..} =
  "INSERT INTO categories(id,name,parent_id) \
  \ VALUES (default, '"
    <> toQuery categoryRawName
    <> "', "
    <> maybeAddParent
    <> ") RETURNING id, name, parent_id"
 where
  maybeAddParent = maybe "default" (fromString . show) categoryRawParentId
