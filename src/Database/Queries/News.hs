{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.News where

import           Control.Monad
import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Models.User
import           Database.Queries.Queries
import           Helpers

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

