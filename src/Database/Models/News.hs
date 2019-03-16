{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Models.News where

import qualified Control.Monad as M (forM_)
import qualified Data.Functor.Identity as I (Identity(..), runIdentity)
import qualified Data.Text as T (Text(..))
import qualified Data.Time as Time (LocalTime(..))
import Database.Models.Author (Author(..))
import Database.Models.Category (CategoryNested(..))
import Database.Models.Tag (Tag(..))
import Database.Models.User (User(..))
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , Query(..)
  , execute
  , query
  , withTransaction
  )
import qualified Database.PostgreSQL.Simple.FromRow as PSQL (FromRow(..), field)
import qualified Database.PostgreSQL.Simple.ToField as PSQL (toField)
import qualified Database.PostgreSQL.Simple.ToRow as PSQL (ToRow(..))
import qualified Database.PostgreSQL.Simple.Types as PSQL (Default(..))
import Database.Queries.Tag (getTagsByNewsId)
import WebServer.Database (Fit(..), Persistent(..))

data News = News
  { newsId :: Integer
  , newsTitle :: T.Text
  , newsDateCreated :: Time.LocalTime
  , newsAuthorId :: Integer
  , newsCategoryId :: Integer
  , newsContent :: T.Text
  , newsMainPhoto :: T.Text
  , newsIsDraft :: Bool
  }

data NewsNested = NewsNested
  { newsNestedId :: Integer
  , newsNestedTitle :: T.Text
  , newsNestedDateCreated :: Time.LocalTime
  , newsNestedContent :: T.Text
  , newsNestedMainPhoto :: T.Text
  , newsNestedIsDraft :: Bool
  , newsNestedAuthorAndUser :: (Author, User)
  , newsNestedCategory :: CategoryNested
  , newsNestedTags :: [Tag]
  , newsNestedPhotos :: [Photo]
  }

data NewsRawT f = NewsRawT
  { newsRawTitle :: f T.Text
  , newsRawAuthorId :: f Integer
  , newsRawCategoryId :: f Integer
  , newsRawContent :: f T.Text
  , newsRawMainPhoto :: f T.Text
  , newsRawTagsIds :: f [Integer]
  , newsRawPhotos :: f [T.Text]
  }

newtype NewsRaw =
  NewsRaw (NewsRawT I.Identity)

newtype NewsRawPartial =
  NewsRawPartial (NewsRawT Maybe)

data TagNews = TagNews
  { tgTagId :: Integer
  , tgNewsId :: Integer
  }

data Photo = Photo
  { photoId :: Integer
  , photoUrl :: T.Text
  , photoNewsId :: Integer
  }

instance PSQL.FromRow News where
  fromRow =
    News <$> PSQL.field <*> PSQL.field <*> PSQL.field <*> PSQL.field <*>
    PSQL.field <*>
    PSQL.field <*>
    PSQL.field <*>
    PSQL.field

instance Persistent News where
  tableName _ = "news"
  select = error "Use selectNews"

instance Fit NewsRaw News

instance Persistent NewsNested where
  tableName _ = error "No table for NewsNested"
  select = error "Use selectNewsNested"
  selectById conn newsId = selectById conn newsId >>= (maybeNestNews conn)

instance Fit NewsRaw NewsNested where
  insert conn newsRaw@(NewsRaw NewsRawT {..}) =
    PSQL.withTransaction conn $ do
      (Just news) <- insert conn newsRaw
      let thisNewsId = newsId news
          tagIds = I.runIdentity newsRawTagsIds
          photoUrls = I.runIdentity newsRawPhotos
      M.forM_ tagIds $ \tagId ->
        PSQL.execute conn insertTagsNewsQuery (tagId, thisNewsId)
      M.forM_ photoUrls $ \photo ->
        PSQL.execute conn insertPhotoQuery (photo, thisNewsId)
      Just <$> nestNews conn news

instance PSQL.ToRow NewsRaw where
  toRow (NewsRaw NewsRawT {..}) =
    [ PSQL.toField PSQL.Default
    , PSQL.toField $ I.runIdentity newsRawTitle
    , PSQL.toField PSQL.Default
    , PSQL.toField $ I.runIdentity newsRawAuthorId
    , PSQL.toField $ I.runIdentity newsRawCategoryId
    , PSQL.toField $ I.runIdentity newsRawContent
    , PSQL.toField $ I.runIdentity newsRawMainPhoto
    , PSQL.toField PSQL.Default
    ]

instance PSQL.FromRow TagNews where
  fromRow = TagNews <$> PSQL.field <*> PSQL.field

instance PSQL.FromRow Photo where
  fromRow = Photo <$> PSQL.field <*> PSQL.field <*> PSQL.field

maybeNestNews :: PSQL.Connection -> Maybe News -> IO (Maybe NewsNested)
maybeNestNews conn = maybe (pure Nothing) (fmap Just . nestNews conn)

nestNews :: PSQL.Connection -> News -> IO NewsNested
nestNews conn News {..} = do
  (Just category) <- selectById conn newsCategoryId -- Pattern matching to Just because db constraints let us to do it
  (Just authorAndUser) <- selectById conn newsAuthorId
  tags <- getTagsByNewsId conn newsId
  photos <- getPhotosByNewsId conn newsId
  pure $
    NewsNested
      { newsNestedId = newsId
      , newsNestedTitle = newsTitle
      , newsNestedDateCreated = newsDateCreated
      , newsNestedContent = newsContent
      , newsNestedMainPhoto = newsMainPhoto
      , newsNestedIsDraft = newsIsDraft
      , newsNestedAuthorAndUser = authorAndUser
      , newsNestedCategory = category
      , newsNestedTags = tags
      , newsNestedPhotos = photos
      }

insertTagsNewsQuery :: PSQL.Query
insertTagsNewsQuery =
  "INSERT INTO tags_news(tag_id, news_id) \
  \ VALUES (?,?) "

insertPhotoQuery :: PSQL.Query
insertPhotoQuery =
  "INSERT INTO photos(id, url, news_id) \
  \ VALUES (default,?,?) "

getPhotosByNewsId :: PSQL.Connection -> Integer -> IO [Photo]
getPhotosByNewsId conn newsId = PSQL.query conn dbQuery [newsId]
  where
    dbQuery =
      "SELECT * FROM photos \
        \WHERE news_id = ?"
