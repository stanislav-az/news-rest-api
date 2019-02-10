{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.News where

import           Control.Monad
import           Database.PostgreSQL.Simple
import           Database.Models.News
import           Database.Models.User
import           Database.Queries.Queries
import           Helpers
import           WebServer.Database
import           WebServer.UrlParser.Filter
import           Data.Maybe                     ( listToMaybe )
import qualified Data.Text                     as T

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
    case newsRawPhotos of
      Nothing     -> pure ()
      Just photos -> deleteOldPhotos >> insertNewPhotos
       where
        deleteOldPhotos =
          execute conn deleteOldPhotosQuery (Only updatingNewsId)
        insertNewPhotos = forM_ photos
          $ \photo -> execute conn insertPhotoQuery (photo, updatingNewsId)
    nestNews conn news

deleteTagsNewsQuery :: Query
deleteTagsNewsQuery = "DELETE FROM tags_news WHERE news_id = ?"

deleteOldPhotosQuery :: Query
deleteOldPhotosQuery = "DELETE FROM photos WHERE news_id = ?"

updateNewsQuery :: NewsRawPartial -> Query
updateNewsQuery (NewsRawPartial NewsRawT {..}) =
  let params = makeQueryParameters
        [ ("title"      , newsRawTitle)
        , ("category_id", texify <$> newsRawCategoryId)
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
  mbUserFromNews <- getAuthorUserByNewsId conn newsId
  pure $ maybe False (\u -> userId user == userId u) mbUserFromNews

getAuthorUserByNewsId :: Connection -> Integer -> IO (Maybe User)
getAuthorUserByNewsId conn newsId = listToMaybe
  <$> query conn dbQuery (Only newsId)
 where
  dbQuery
    = "SELECT u.id, u.name, u.surname, u.avatar, u.date_created, u.is_admin FROM users u \
  \JOIN authors a ON a.user_id = u.id \
  \JOIN news n ON n.author_id = a.id \
  \WHERE n.id = ?"

selectNewsNested :: Connection -> (Limit, Offset) -> Filter -> IO [NewsNested]
selectNewsNested conn pagination filter =
  selectNews conn pagination filter >>= (mapM (nestNews conn))

selectNews :: Connection -> (Limit, Offset) -> Filter -> IO [News]
selectNews conn (Limit limit, Offset offset) filter = do
  print dbQuery
  query conn dbQuery [limit, offset]
 where
  dbQuery =
    "SELECT news_id, news_title, news_date_created, news_author_id, \
    \news_category_id, news_content, news_main_photo, news_is_draft \
    \FROM filterable_news \
    \WHERE true "
      <> filterToQuery filter
      <> " LIMIT ? OFFSET ? ;"

filterToQuery :: Filter -> Query
filterToQuery Filter {..} =
  maybeQuery dateCreatedToQuery filterDateCreated
    <> maybeQuery authorNameToQuery  filterAuthorName
    <> maybeQuery categoryIdToQuery  filterCategoryId
    <> maybeQuery tagIdsToQuery      filterTagIds
    <> maybeQuery newsTitleToQuery   filterNewsTitleHas
    <> maybeQuery newsContentToQuery filterNewsContentHas
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

maybeQuery :: (a -> Query) -> Maybe a -> Query
maybeQuery = maybe ""

findNewsNested :: Connection -> (Limit, Offset) -> T.Text -> IO [NewsNested]
findNewsNested conn pagination searchText =
  findNews conn pagination searchText >>= (mapM (nestNews conn))

findNews :: Connection -> (Limit, Offset) -> T.Text -> IO [News]
findNews conn (Limit limit, Offset offset) searchText = do
  print dbQuery
  query conn dbQuery [limit, offset]
 where
  dbQuery =
    "SELECT DISTINCT news_id, news_title, news_date_created, news_author_id, \
    \news_category_id, news_content, news_main_photo, news_is_draft \
    \FROM searchable_news \
    \WHERE false "
      <> " OR news_content ~* '" <> searchQuery <> "' "
      <> " OR news_author_name ~* '" <> searchQuery <> "' "
      <> " OR news_category_name ~* '" <> searchQuery <> "' "
      <> " OR news_tag_name ~* '" <> searchQuery <> "' "
      <> " LIMIT ? OFFSET ? ;" 
  searchQuery = textToQuery searchText
