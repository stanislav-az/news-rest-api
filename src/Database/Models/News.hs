module Database.Models.News where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text
import           Data.Time
import           Data.Functor.Identity

data News = News {
  newsId :: Integer,
  newsTitle :: Text,
  newsDateCreated :: LocalTime,
  newsAuthorId :: Integer,
  newsCategoryId :: Integer,
  newsContent :: Text,
  newsMainPhoto :: Text,
  newsIsDraft :: Bool
}

instance FromRow News where
  fromRow =
    News
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data NewsRawT f = NewsRawT {
  newsRawTitle :: f Text,
  newsRawAuthorId :: f Integer,
  newsRawCategoryId :: f Integer,
  newsRawContent :: f Text,
  newsRawMainPhoto :: f Text,
  newsRawTagsIds :: f [Integer]
}

newtype NewsRaw = NewsRaw (NewsRawT Identity)

newtype NewsRawPartial = NewsRawPartial (NewsRawT Maybe)

data TagNews = TagNews {
  tgTagId :: Integer,
  tgNewsId :: Integer
}

instance FromRow TagNews where
  fromRow = TagNews <$> field <*> field
