{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.News where

import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Connection
import           Database.Queries.Queries
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T
import           Data.Functor.Identity
import           Control.Monad

getNewsList :: IO [News]
getNewsList = getList "news"

addNewsToDB :: NewsRaw -> IO News
addNewsToDB (NewsRaw NewsRawT {..}) =
  bracket (connect connectInfo) close $ \conn -> withTransaction conn $ do
    (news : _) <- query
      conn
      insertNewsQuery
      ( runIdentity newsRawTitle
      , runIdentity newsRawAuthorId
      , runIdentity newsRawCategoryId
      , runIdentity newsRawContent
      , runIdentity newsRawMainPhoto
      , runIdentity newsRawIsDraft
      )
    let thisNewsId = newsId news
        tagIds     = runIdentity newsRawTagsIds
    forM_ tagIds
      $ \tagId -> execute conn insertTagsNewsQuery (tagId, thisNewsId)
    pure news

insertNewsQuery :: Query
insertNewsQuery =
  "INSERT INTO news(news_id, title, date_created, author_id, category_id, content, main_photo, is_draft) \
  \ VALUES (default,?,default,?,?,?,?,?) \
  \ RETURNING news_id, title, date_created, author_id, category_id, content, main_photo, is_draft"

insertTagsNewsQuery :: Query
insertTagsNewsQuery =
  "INSERT INTO tags_news(tag_id, news_id) \
  \ VALUES (?,?) "
