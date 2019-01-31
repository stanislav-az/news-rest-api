{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.News where

import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Models.User
import           Database.Connection
import           Database.Queries.Queries
import qualified Data.Text                     as T
import           Data.Functor.Identity
import           Control.Monad
import           Helpers

getNewsList :: Connection -> IO [News]
getNewsList conn = getList conn "news"

addNewsToDB :: Connection -> NewsRaw -> IO News
addNewsToDB conn (NewsRaw NewsRawT {..}) = withTransaction conn $ do
  (news : _) <- query
    conn
    insertNewsQuery
    ( runIdentity newsRawTitle
    , runIdentity newsRawAuthorId
    , runIdentity newsRawCategoryId
    , runIdentity newsRawContent
    , runIdentity newsRawMainPhoto
    )
  let thisNewsId = newsId news
      tagIds     = runIdentity newsRawTagsIds
  forM_ tagIds $ \tagId -> execute conn insertTagsNewsQuery (tagId, thisNewsId)
  pure news

publishNews :: Connection -> Integer -> IO News
publishNews conn newsId = head <$> query conn publishNewsQuery (Only newsId)

publishNewsQuery :: Query
publishNewsQuery =
  "UPDATE news SET is_draft = false \
  \WHERE news_id = ? \
  \RETURNING news_id, title, date_created, author_id, category_id, content, main_photo, is_draft"

updateNews :: Connection -> Integer -> NewsRawPartial -> IO News
updateNews conn updatingNewsId newsPartial@(NewsRawPartial NewsRawT {..}) =
  withTransaction conn $ do
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
  "INSERT INTO news(news_id, title, date_created, author_id, category_id, content, main_photo, is_draft) \
  \ VALUES (default,?,default,?,?,?,?,default) \
  \ RETURNING news_id, title, date_created, author_id, category_id, content, main_photo, is_draft"

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
    <> "RETURNING news_id, title, date_created, author_id, category_id, content, main_photo, is_draft"

isAuthorOfNews :: Connection -> User -> Integer -> IO Bool
isAuthorOfNews conn user newsId = do
  userFromNews <- getAuthorUseByNewsId conn newsId
  let userIdFromUser = userId user
      userIdFromNews = userId userFromNews
  pure $ userIdFromUser == userIdFromNews

getAuthorUseByNewsId :: Connection -> Integer -> IO User
getAuthorUseByNewsId conn newsId = head <$> query conn q (Only newsId)
 where
  q
    = "SELECT u.user_id, u.name, u.surname, u.avatar, u.date_created, u.is_admin FROM users u \
  \JOIN authors a ON a.user_id = u.user_id \
  \JOIN news n ON n.author_id = a.author_id \
  \WHERE news_id = ?"

