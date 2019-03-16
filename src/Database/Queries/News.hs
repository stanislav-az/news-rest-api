{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.News where

import qualified Control.Monad as M (forM_)
import qualified Data.Maybe as MB (listToMaybe)
import qualified Data.Text as T (Text(..))
import Database.Models.News
  ( News(..)
  , NewsNested(..)
  , NewsRawPartial(..)
  , NewsRawT(..)
  , insertPhotoQuery
  , insertTagsNewsQuery
  , nestNews
  )
import Database.Models.User (User(..))
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , Query(..)
  , execute
  , query
  , withTransaction
  )
import Database.Queries.Queries
  ( bsToQuery
  , makeQueryParameters
  , showToQuery
  , textToQuery
  )
import Helpers (texify)
import WebServer.Database (Limit(..), Offset(..))
import WebServer.UrlParser.Filter (DateCreated(..), Filter(..), TagIds(..))
import WebServer.UrlParser.Sorter (Sorter(..))

publishNews :: PSQL.Connection -> Integer -> IO NewsNested
publishNews conn newsId =
  (head <$> PSQL.query conn publishNewsQuery [newsId]) >>= nestNews conn

publishNewsQuery :: PSQL.Query
publishNewsQuery =
  "UPDATE news SET is_draft = false \
  \WHERE id = ? \
  \RETURNING id, title, date_created, author_id, category_id, content, main_photo, is_draft"

updateNews :: PSQL.Connection -> Integer -> NewsRawPartial -> IO NewsNested
updateNews conn updatingNewsId newsPartial@(NewsRawPartial NewsRawT {..}) =
  PSQL.withTransaction conn $ do
    (news:_) <- PSQL.query conn (updateNewsQuery newsPartial) [updatingNewsId]
    case newsRawTagsIds of
      Nothing -> pure ()
      Just tagIds -> deleteOldTagConnections >> makeNewTagConnections
        where deleteOldTagConnections =
                PSQL.execute conn deleteTagsNewsQuery [updatingNewsId]
              makeNewTagConnections =
                M.forM_ tagIds $ \tagId ->
                  PSQL.execute conn insertTagsNewsQuery (tagId, updatingNewsId)
    case newsRawPhotos of
      Nothing -> pure ()
      Just photos -> deleteOldPhotos >> insertNewPhotos
        where deleteOldPhotos =
                PSQL.execute conn deleteOldPhotosQuery [updatingNewsId]
              insertNewPhotos =
                M.forM_ photos $ \photo ->
                  PSQL.execute conn insertPhotoQuery (photo, updatingNewsId)
    nestNews conn news

deleteTagsNewsQuery :: PSQL.Query
deleteTagsNewsQuery = "DELETE FROM tags_news WHERE news_id = ?"

deleteOldPhotosQuery :: PSQL.Query
deleteOldPhotosQuery = "DELETE FROM photos WHERE news_id = ?"

updateNewsQuery :: NewsRawPartial -> PSQL.Query
updateNewsQuery (NewsRawPartial NewsRawT {..}) =
  let params =
        makeQueryParameters
          [ ("title", newsRawTitle)
          , ("category_id", texify <$> newsRawCategoryId)
          , ("content", newsRawContent)
          , ("main_photo", newsRawMainPhoto)
          ]
   in "UPDATE news SET " <> params <> "WHERE id = ? " <>
      "RETURNING id, title, date_created, author_id, category_id, content, main_photo, is_draft"

isAuthorOfNews :: PSQL.Connection -> User -> Integer -> IO Bool
isAuthorOfNews conn user newsId = do
  mbUserFromNews <- getAuthorUserByNewsId conn newsId
  pure $ maybe False (\u -> userId user == userId u) mbUserFromNews

getAuthorUserByNewsId :: PSQL.Connection -> Integer -> IO (Maybe User)
getAuthorUserByNewsId conn newsId =
  MB.listToMaybe <$> PSQL.query conn dbQuery [newsId]
  where
    dbQuery =
      "SELECT u.id, u.name, u.surname, u.avatar, u.date_created, u.is_admin FROM users u \
  \JOIN authors a ON a.user_id = u.id \
  \JOIN news n ON n.author_id = a.id \
  \WHERE n.id = ?"

selectNewsNested ::
     PSQL.Connection
  -> (Limit, Offset)
  -> Filter
  -> Maybe Sorter
  -> IO [NewsNested]
selectNewsNested conn pagination filter sorter =
  selectNews conn pagination filter sorter >>= (mapM (nestNews conn))

selectNews ::
     PSQL.Connection -> (Limit, Offset) -> Filter -> Maybe Sorter -> IO [News]
selectNews conn (Limit limit, Offset offset) filter sorter =
  PSQL.query conn dbQuery [limit, offset]
  where
    dbQuery =
      "SELECT news_id, news_title, news_date_created, news_author_id, \
    \news_category_id, news_content, news_main_photo, news_is_draft \
    \FROM filterable_news \
    \WHERE true " <>
      filterToQuery filter <>
      maybeQuery sorterToQuery sorter <>
      " LIMIT ? OFFSET ? ;"

filterToQuery :: Filter -> PSQL.Query
filterToQuery Filter {..} =
  maybeQuery dateCreatedToQuery filterDateCreated <>
  maybeQuery authorNameToQuery filterAuthorName <>
  maybeQuery categoryIdToQuery filterCategoryId <>
  maybeQuery tagIdsToQuery filterTagIds <>
  maybeQuery newsTitleToQuery filterNewsTitleHas <>
  maybeQuery newsContentToQuery filterNewsContentHas
  where
    dateCreatedToQuery (CreatedAt day) =
      " AND date_trunc('day', news_date_created) = '" <> showToQuery day <> "' "
    dateCreatedToQuery (CreatedAtLt day) =
      " AND date_trunc('day', news_date_created) < '" <> showToQuery day <> "' "
    dateCreatedToQuery (CreatedAtGt day) =
      " AND date_trunc('day', news_date_created) > '" <> showToQuery day <> "' "
    authorNameToQuery name =
      " AND news_author_name = '" <> bsToQuery name <> "' "
    categoryIdToQuery id = " AND news_category_id = " <> showToQuery id
    tagIdsToQuery (TagId id) = " AND news_tag_ids @> ARRAY" <> showToQuery [id]
    tagIdsToQuery (TagsIn tags) =
      " AND news_tag_ids && ARRAY" <> showToQuery tags
    tagIdsToQuery (TagsAll tags) =
      " AND news_tag_ids @> ARRAY" <> showToQuery tags
    newsTitleToQuery title = " AND news_title ~* '" <> bsToQuery title <> "' "
    newsContentToQuery content =
      " AND news_content ~* '" <> bsToQuery content <> "' "

maybeQuery :: (a -> PSQL.Query) -> Maybe a -> PSQL.Query
maybeQuery = maybe ""

findNewsNested ::
     PSQL.Connection
  -> (Limit, Offset)
  -> T.Text
  -> Maybe Sorter
  -> IO [NewsNested]
findNewsNested conn pagination searchText sorter =
  findNews conn pagination searchText sorter >>= (mapM (nestNews conn))

findNews ::
     PSQL.Connection -> (Limit, Offset) -> T.Text -> Maybe Sorter -> IO [News]
findNews conn (Limit limit, Offset offset) searchText sorter =
  PSQL.query conn dbQuery [limit, offset]
  where
    dbQuery =
      "SELECT DISTINCT news_id, news_title, news_date_created, news_author_id, \
    \news_category_id, news_content, news_main_photo, news_is_draft \
    \FROM searchable_news \
    \WHERE false " <>
      " OR news_content ~* '" <>
      searchQuery <>
      "' " <>
      " OR news_author_name ~* '" <>
      searchQuery <>
      "' " <>
      " OR news_category_name ~* '" <>
      searchQuery <>
      "' " <>
      " OR news_tag_name ~* '" <>
      searchQuery <>
      "' " <>
      maybeQuery sorterToQuery sorter <>
      " LIMIT ? OFFSET ? ;"
    searchQuery = textToQuery searchText

sorterToQuery :: Sorter -> PSQL.Query
sorterToQuery Author = " ORDER BY news_author_name "
sorterToQuery Category = " ORDER BY news_category_name "
sorterToQuery Date = " ORDER BY news_date_created "
sorterToQuery Photos = " ORDER BY photos_num "
