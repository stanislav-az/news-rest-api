{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.Category where

import qualified Database.PostgreSQL.Simple    as PSQL
                                                ( Query(..)
                                                , Connection(..)
                                                , query_
                                                )
import qualified Database.PostgreSQL.Simple.FromRow
                                               as PSQL
                                                ( FromRow(..)
                                                , field
                                                )
import qualified Data.Text                     as T
                                                ( Text(..) )
import qualified Data.Maybe                    as MB
                                                ( listToMaybe )
import           WebServer.Database             ( Persistent(..)
                                                , Fit(..)
                                                )
import           Database.Queries.Queries       ( showToQuery
                                                , textToQuery
                                                )

data Category = Category {
  categoryId :: Integer,
  categoryName :: T.Text,
  categoryParentId :: Maybe Integer
} deriving Show

instance PSQL.FromRow Category where
  fromRow = Category <$> PSQL.field <*> PSQL.field <*> PSQL.field

instance Persistent Category where
  tableName _ = "categories"

data CategoryNested = CategoryNested Integer T.Text CategoryNested | Parent Integer T.Text deriving Show

instance Persistent CategoryNested where
  tableName _ = error "No table for CategoryNested"

  select conn pagination =
    select conn pagination >>= (mapM (nestCategory conn))

  selectById conn categoryId =
    selectById conn categoryId >>= (maybeNestCategory conn)

instance Fit CategoryRaw CategoryNested where
  insert conn cr =
    (MB.listToMaybe <$> PSQL.query_ conn (insertCategoryQuery cr))
      >>= (maybeNestCategory conn)

data CategoryRaw = CategoryRaw {
  categoryRawName :: T.Text,
  categoryRawParentId :: Maybe Integer
}

data CategoryRawPartial = CategoryRawPartial {
  categoryRawPartialName :: Maybe T.Text,
  categoryRawPartialParentId :: Maybe Integer
}

maybeNestCategory
  :: PSQL.Connection -> Maybe Category -> IO (Maybe CategoryNested)
maybeNestCategory conn = maybe (pure Nothing) (fmap Just . nestCategory conn)

nestCategory :: PSQL.Connection -> Category -> IO CategoryNested
nestCategory conn (Category id name Nothing        ) = pure $ Parent id name
nestCategory conn (Category id name (Just parentId)) = do
  (Just parent) <- selectById conn parentId -- Pattern matching to Just because db constraints let us to do it
  parentNested  <- nestCategory conn parent
  pure $ CategoryNested id name parentNested

insertCategoryQuery :: CategoryRaw -> PSQL.Query
insertCategoryQuery CategoryRaw {..} =
  "INSERT INTO categories(id,name,parent_id) \
  \ VALUES (default, '"
    <> textToQuery categoryRawName
    <> "', "
    <> maybeAddParent
    <> ") RETURNING id, name, parent_id"
  where maybeAddParent = maybe "default" showToQuery categoryRawParentId
