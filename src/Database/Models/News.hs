{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.News where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           Data.Text
import           Data.Time
import           Data.Functor.Identity
import           WebServer.Database
import           Database.Models.Category
import           Database.Models.User
import           Database.Models.Author
import           Database.Models.Tag
import           Database.Queries.Tag
import           Control.Monad

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

instance Persistent News where
  tableName _ = "news"

  select = error "Use selectNews"

instance Fit NewsRaw News where

data NewsNested = NewsNested {
  newsNestedId :: Integer,
  newsNestedTitle :: Text,
  newsNestedDateCreated :: LocalTime,
  newsNestedContent :: Text,
  newsNestedMainPhoto :: Text,
  newsNestedIsDraft :: Bool,
  newsNestedAuthorAndUser :: (Author,User),
  newsNestedCategory :: CategoryNested,
  newsNestedTags :: [Tag],
  newsNestedPhotos :: [Photo]
}

instance Persistent NewsNested where
  tableName _ = error "No table for NewsNested"

  select = error "Use selectNewsNested"

  selectById conn newsId = selectById conn newsId >>= (maybeNestNews conn)

instance Fit NewsRaw NewsNested where
  insert conn newsRaw@(NewsRaw NewsRawT {..}) = withTransaction conn $ do
    (Just news) <- insert conn newsRaw
    let thisNewsId = newsId news
        tagIds     = runIdentity newsRawTagsIds
        photoUrls  = runIdentity newsRawPhotos
    forM_ tagIds
      $ \tagId -> execute conn insertTagsNewsQuery (tagId, thisNewsId)
    forM_ photoUrls
      $ \photo -> execute conn insertPhotextToQuery (photo, thisNewsId)
    Just <$> nestNews conn news

data NewsRawT f = NewsRawT {
  newsRawTitle :: f Text,
  newsRawAuthorId :: f Integer,
  newsRawCategoryId :: f Integer,
  newsRawContent :: f Text,
  newsRawMainPhoto :: f Text,
  newsRawTagsIds :: f [Integer],
  newsRawPhotos :: f [Text]
}

newtype NewsRaw = NewsRaw (NewsRawT Identity)

instance ToRow NewsRaw where
  toRow (NewsRaw NewsRawT {..}) =
    [ toField Default
    , toField $ runIdentity newsRawTitle
    , toField Default
    , toField $ runIdentity newsRawAuthorId
    , toField $ runIdentity newsRawCategoryId
    , toField $ runIdentity newsRawContent
    , toField $ runIdentity newsRawMainPhoto
    , toField Default
    ]

newtype NewsRawPartial = NewsRawPartial (NewsRawT Maybe)

data TagNews = TagNews {
  tgTagId :: Integer,
  tgNewsId :: Integer
}

instance FromRow TagNews where
  fromRow = TagNews <$> field <*> field

data Photo = Photo {
  photoId :: Integer,
  photoUrl :: Text,
  photoNewsId :: Integer
}

instance FromRow Photo where
  fromRow = Photo <$> field <*> field <*> field

maybeNestNews :: Connection -> Maybe News -> IO (Maybe NewsNested)
maybeNestNews conn = maybe (pure Nothing) (fmap Just . nestNews conn)

nestNews :: Connection -> News -> IO NewsNested
nestNews conn News {..} = do
  (Just category     ) <- selectById conn newsCategoryId -- Pattern matching to Just because db constraints let us to do it
  (Just authorAndUser) <- selectById conn newsAuthorId
  tags                 <- getTagsByNewsId conn newsId
  photos               <- getPhotosByNewsId conn newsId
  pure $ NewsNested { newsNestedId            = newsId
                    , newsNestedTitle         = newsTitle
                    , newsNestedDateCreated   = newsDateCreated
                    , newsNestedContent       = newsContent
                    , newsNestedMainPhoto     = newsMainPhoto
                    , newsNestedIsDraft       = newsIsDraft
                    , newsNestedAuthorAndUser = authorAndUser
                    , newsNestedCategory      = category
                    , newsNestedTags          = tags
                    , newsNestedPhotos        = photos
                    }

insertTagsNewsQuery :: Query
insertTagsNewsQuery =
  "INSERT INTO tags_news(tag_id, news_id) \
  \ VALUES (?,?) "

insertPhotextToQuery :: Query
insertPhotextToQuery =
  "INSERT INTO photos(id, url, news_id) \
  \ VALUES (default,?,?) "

getPhotosByNewsId :: Connection -> Integer -> IO [Photo]
getPhotosByNewsId conn newsId = query conn dbQuery (Only newsId)
  where dbQuery = "SELECT * FROM photos \
        \WHERE news_id = ?"
