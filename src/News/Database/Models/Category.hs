{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module News.Database.Models.Category where

import qualified Data.Maybe as MB (listToMaybe)
import qualified Data.Text as T (Text(..))
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , Query(..)
  , query_
  )
import qualified Database.PostgreSQL.Simple.FromRow as PSQL (FromRow(..), field)
import News.Database.Queries.Queries (showToQuery, textToQuery)
import News.WebServer.Database (Fit(..), Persistent(..))

data Category = Category
  { categoryId :: Integer
  , categoryName :: T.Text
  , categoryParentId :: Maybe Integer
  } deriving (Show)

data CategoryNested
  = CategoryNested Integer
                   T.Text
                   CategoryNested
  | Parent Integer
           T.Text
  deriving (Show)

data CategoryRaw = CategoryRaw
  { categoryRawName :: T.Text
  , categoryRawParentId :: Maybe Integer
  }

data CategoryRawPartial = CategoryRawPartial
  { categoryRawPartialName :: Maybe T.Text
  , categoryRawPartialParentId :: Maybe Integer
  }

instance PSQL.FromRow Category where
  fromRow = Category <$> PSQL.field <*> PSQL.field <*> PSQL.field

instance Persistent Category where
  tableName _ = "categories"

instance Persistent CategoryNested where
  tableName _ = error "No table for CategoryNested"
  select conn pagination = select conn pagination >>= mapM (nestCategory conn)
  selectById conn categoryId =
    selectById conn categoryId >>= maybeNestCategory conn

instance Fit CategoryRaw CategoryNested where
  insert conn cr =
    (MB.listToMaybe <$> PSQL.query_ conn (insertCategoryQuery cr)) >>=
    maybeNestCategory conn

maybeNestCategory ::
     PSQL.Connection -> Maybe Category -> IO (Maybe CategoryNested)
maybeNestCategory conn = maybe (pure Nothing) (fmap Just . nestCategory conn)

nestCategory :: PSQL.Connection -> Category -> IO CategoryNested
nestCategory conn (Category id name Nothing) = pure $ Parent id name
nestCategory conn (Category id name (Just parentId)) = do
  (Just parent) <- selectById conn parentId -- Pattern matching to Just because db constraints let us to do it
  parentNested <- nestCategory conn parent
  pure $ CategoryNested id name parentNested

insertCategoryQuery :: CategoryRaw -> PSQL.Query
insertCategoryQuery CategoryRaw {..} =
  "INSERT INTO categories(id,name,parent_id) \
  \ VALUES (default, '" <>
  textToQuery categoryRawName <>
  "', " <>
  maybeAddParent <>
  ") RETURNING id, name, parent_id"
  where
    maybeAddParent = maybe "default" showToQuery categoryRawParentId
