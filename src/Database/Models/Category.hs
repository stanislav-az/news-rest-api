module Database.Models.Category where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text
import           Data.Time


data Category = Category {
  categoryId :: Integer,
  categoryName :: Text,
  categoryParentId :: Maybe Integer
} deriving Show

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field

data CategoryNested = CategoryNested Integer Text CategoryNested | Parent Integer Text

data CategoryRaw = CategoryRaw {
  categoryRawName :: Text,
  categoryRawParentId :: Maybe Integer
}

data CategoryRawPartial = CategoryRawPartial {
  categoryRawPartialName :: Maybe Text,
  categoryRawPartialParentId :: Maybe Integer
}