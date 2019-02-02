{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.News where

import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Models.User
import           Database.Queries.Category
import           Database.Queries.Tag
import           Database.Queries.Author
import           Database.Connection
import           Database.Queries.Queries
import qualified Data.Text                     as T
import           Data.Functor.Identity
import           Control.Monad
import           Helpers

getNewsList :: Connection -> IO [NewsNested]
getNewsList conn = getList conn "news" >>= (mapM (nestNews conn))

nestNews :: Connection -> News -> IO NewsNested
nestNews conn News {..} = do
  category <- getCategoryById conn newsCategoryId
  author   <- getAuthorNestedById conn newsAuthorId
  tags     <- getTagsByNewsId conn newsId
  pure $ NewsNested { newsNestedId          = newsId
                    , newsNestedTitle       = newsTitle
                    , newsNestedDateCreated = newsDateCreated
                    , newsNestedContent     = newsContent
                    , newsNestedMainPhoto   = newsMainPhoto
                    , newsNestedIsDraft     = newsIsDraft
                    , newsNestedAuthor      = author
                    , newsNestedCategory    = category
                    , newsNestedTags        = tags
                    }

addNewsToDB :: Connection -> NewsRaw -> IO NewsNested
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
  nestNews conn news

publishNews :: Connection -> Integer -> IO NewsNested
publishNews conn newsId =
  (head <$> query conn publishNewsQuery (Only newsId)) >>= (nestNews conn)

publishNewsQuery :: Query
publishNewsQuery =
  "UPDATE news SET is_draft = false \
  \WHERE id = ? \
  \RETURNING id, title, date_created, author_id, category_id, content, main_photo, is_draft"

updateNews :: Connection -> Integer -> NewsRawPartial -> IO NewsNested
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
    nestNews conn news

insertNewsQuery :: Query
insertNewsQuery =
  "INSERT INTO news(id, title, date_created, author_id, category_id, content, main_photo, is_draft) \
  \ VALUES (default,?,default,?,?,?,?,default) \
  \ RETURNING id, title, date_created, author_id, category_id, content, main_photo, is_draft"

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
    <> "WHERE id = ? "
    <> "RETURNING id, title, date_created, author_id, category_id, content, main_photo, is_draft"

isAuthorOfNews :: Connection -> User -> Integer -> IO Bool
isAuthorOfNews conn user newsId = do
  userFromNews <- getAuthorUserByNewsId conn newsId
  let userIdFromUser = userId user
      userIdFromNews = userId userFromNews
  pure $ userIdFromUser == userIdFromNews

getAuthorUserByNewsId :: Connection -> Integer -> IO User
getAuthorUserByNewsId conn newsId = head <$> query conn q (Only newsId)
 where
  q
    = "SELECT u.id, u.name, u.surname, u.avatar, u.date_created, u.is_admin FROM users u \
  \JOIN authors a ON a.user_id = u.id \
  \JOIN news n ON n.author_id = a.id \
  \WHERE n.id = ?"

