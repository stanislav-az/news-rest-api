{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.News where

import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Models.User
import           Database.Connection
import           Database.Queries.Queries
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T
import           Data.Functor.Identity
import           Control.Monad
import           Helpers

getNewsList :: IO [News]
getNewsList = getList "news"

addNewsToDB :: NewsRaw -> IO News
addNewsToDB (NewsRaw NewsRawT {..}) =
  bracket (connect connectInfo) close $ \conn -> withTransaction conn $ do
    (news : _) <- query
      conn
      insertNewsQuery
      ( runIdentity newsRawTitle
      , runIdentity newsRawUserId
      , runIdentity newsRawCategoryId
      , runIdentity newsRawContent
      , runIdentity newsRawMainPhoto
      )
    let thisNewsId = newsId news
        tagIds     = runIdentity newsRawTagsIds
    forM_ tagIds
      $ \tagId -> execute conn insertTagsNewsQuery (tagId, thisNewsId)
    pure news

publishNews :: Integer -> IO News
publishNews newsId = bracket (connect connectInfo) close
  $ \conn -> head <$> query conn publishNewsQuery (Only newsId)

publishNewsQuery :: Query
publishNewsQuery =
  "UPDATE news SET is_draft = false \
  \WHERE news_id = ? \
  \RETURNING news_id, title, date_created, user_id, category_id, content, main_photo, is_draft"

updateNews :: Integer -> NewsRawPartial -> IO News
updateNews updatingNewsId newsPartial@(NewsRawPartial NewsRawT {..}) =
  bracket (connect connectInfo) close $ \conn -> withTransaction conn $ do
    (news : _) <- query conn (updateNewsQuery newsPartial) (Only updatingNewsId)
    case newsRawTagsIds of
      Nothing     -> pure ()
      Just tagIds -> deleteOldTagConnections >> makeNewTagConnections
       where
        deleteOldTagConnections =
          execute conn deleteTagsNewsQuery (Only updatingNewsId)
        makeNewTagConnections = forM_ tagIds
          $ \tagId -> execute conn insertTagsNewsQuery (tagId, updatingNewsId)
    pure news

insertNewsQuery :: Query
insertNewsQuery =
  "INSERT INTO news(news_id, title, date_created, user_id, category_id, content, main_photo, is_draft) \
  \ VALUES (default,?,default,?,?,?,?,default) \
  \ RETURNING news_id, title, date_created, user_id, category_id, content, main_photo, is_draft"

insertTagsNewsQuery :: Query
insertTagsNewsQuery =
  "INSERT INTO tags_news(tag_id, news_id) \
  \ VALUES (?,?) "

deleteTagsNewsQuery :: Query
deleteTagsNewsQuery = "DELETE FROM tags_news WHERE news_id = ?"

updateNewsQuery :: NewsRawPartial -> Query
updateNewsQuery (NewsRawPartial NewsRawT {..}) =
  let params = makeQueryParameters
        [ ("title"      , newsRawTitle)
        , ("category_id", integerToText <$> newsRawCategoryId)
        , ("content"    , newsRawContent)
        , ("main_photo" , newsRawMainPhoto)
        ]
  in
    "UPDATE news SET "
    <> params
    <> "WHERE news_id = ? "
    <> "RETURNING news_id, title, date_created, user_id, category_id, content, main_photo, is_draft"

isAuthorOfNews :: User -> Integer -> IO Bool
isAuthorOfNews user newsId = do
  news <- getNewsById newsId
  let uid1 = newsUserId news
      uid2 = userId user
  pure $ uid1 == uid2

getNewsById :: Integer -> IO News
getNewsById newsId = bracket (connect connectInfo) close
  $ \conn -> head <$> query conn q (Only newsId)
  where q = "SELECT * FROM news WHERE news_id = ?"
