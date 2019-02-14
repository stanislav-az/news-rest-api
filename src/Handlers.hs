{-# LANGUAGE FlexibleContexts #-}

module Handlers where

import           Network.Wai
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Network.HTTP.Types            as HTTP
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Control.Exception             as E
import           Data.Aeson
import           Data.Proxy
import           Serializer.Author
import           Serializer.Tag
import           Serializer.Category
import           Serializer.News
import           Serializer.Commentary
import           Database.Queries.Author
import           Database.Queries.Tag
import           Database.Queries.Category
import           Database.Queries.News
import           Database.Queries.Commentary
import           Helpers
import           WebServer.HandlerMonad
import           WebServer.MonadDatabase
import           WebServer.HandlerClass
import           WebServer.Error
import qualified WebServer.Database            as D
import           WebServer.UrlParser.Pagination
import           WebServer.UrlParser.Filter
import           WebServer.UrlParser.Dynamic
import           WebServer.UrlParser.Sorter
import qualified Config                        as C
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T



create
  :: ( MonadReader HandlerEnv m
     , MonadError HandlerError m
     , MonadHTTP m
     , ToJSON a1
     , FromJSON t
     )
  => Inserter t1 m a
  -> (t -> t1)
  -> (a -> a1)
  -> m Response
create inserter requestToEntity entityToResponse = do
  req  <- asks hRequest
  conn <- asks hConnection
  body <- getRequestBody req
  let createEntityData = eitherDecode body
  either (throwError . ParseError) (createEntity conn) createEntityData
 where
  createEntity conn entityData = do
    eEntity <- inserter $ requestToEntity entityData
    entity  <- either (throwError . PSQLError) pure eEntity
    let entityJSON = encode $ entityToResponse entity
    okResponseWithJSONBody entityJSON

list
  :: ( MonadReader HandlerEnv m
     , MonadError HandlerError m
     , MonadHTTP m
     , ToJSON b
     )
  => Selector m a
  -> (a -> b)
  -> m Response
list selector entityToResponse = do
  req      <- asks hRequest
  maxLimit <- asks hMaxLimit
  let pagination = getLimitOffset maxLimit req
  eEntities <- selector pagination
  entities  <- either (throwError . PSQLError) pure eEntities
  let responseEntities  = entityToResponse <$> entities
      printableEntities = encode responseEntities
  okResponseWithJSONBody printableEntities

remove
  :: (MonadReader HandlerEnv m, MonadError HandlerError m, MonadHTTP m)
  => Deleter m
  -> m Response
remove deleter = do
  dpMap    <- asks hDynamicPathsMap
  entityId <- either throwParseError pure $ getIdFromUrl dpMap
  if entityId == 0
    then throwError Forbidden
    else do
      res <- deleter entityId
      either (throwError . PSQLError) pure res
      okResponse

updateAuthorHandler :: Handler
updateAuthorHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateAuthorData = eitherDecode body
  authorId <- either throwParseError pure $ getIdFromUrl dpMap
  conn     <- asks hConnection
  either (throwError . ParseError)
         (goUpdateAuthor conn authorId)
         updateAuthorData
 where
  goUpdateAuthor conn authorId authorData = do
    let partial = requestToUpdateAuthor authorData
    eAuthor <- liftIO $ E.try $ updateAuthor conn authorId partial
    author  <- either throwPSQLError pure eAuthor
    let authorJSON = encode $ authorToUpdateResponse author
    okResponseWithJSONBody authorJSON

updateTagHandler :: Handler
updateTagHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateTagData = eitherDecode body
  tagId <- either throwParseError pure $ getIdFromUrl dpMap
  conn  <- asks hConnection
  either (throwError . ParseError) (goUpdateTag conn tagId) updateTagData
 where
  goUpdateTag conn tagId tagData = do
    let partial = requestToUpdateTag tagData
    eTag <- liftIO $ E.try $ updateTag conn tagId partial
    tag  <- either throwPSQLError pure eTag
    let tagJSON = encode $ tagToResponse tag
    okResponseWithJSONBody tagJSON

updateCategoryHandler :: Handler
updateCategoryHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateCategoryData = eitherDecode body
  categoryId <- either throwParseError pure $ getIdFromUrl dpMap
  conn       <- asks hConnection
  either (throwError . ParseError)
         (goUpdateCategory conn categoryId)
         updateCategoryData
 where
  goUpdateCategory conn categoryId categoryData = do
    let partial = requestToUpdateCategory categoryData
    eCategory <- liftIO $ E.try $ updateCategory conn categoryId partial
    category  <- either throwPSQLError pure eCategory
    let categoryJSON = encode $ categoryNestedToResponse category
    okResponseWithJSONBody categoryJSON

updateNewsHandler :: Handler
updateNewsHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateNewsData = eitherDecode body
  newsId <- either throwParseError pure $ getIdFromUrl dpMap
  conn   <- asks hConnection
  either (throwError . ParseError) (goUpdateNews conn newsId) updateNewsData
 where
  goUpdateNews conn newsId newsData = do
    let partial = requestToUpdateNews newsData
    eNews <- liftIO $ E.try $ updateNews conn newsId partial
    news  <- either throwPSQLError pure eNews
    let newsJSON = encode $ newsToResponse news
    okResponseWithJSONBody newsJSON

publishNewsHandler :: Handler
publishNewsHandler = do
  req    <- asks hRequest
  dpMap  <- asks hDynamicPathsMap
  body   <- getRequestBody req
  newsId <- either throwParseError pure $ getIdFromUrl dpMap
  conn   <- asks hConnection
  eNews  <- liftIO $ E.try $ publishNews conn newsId
  news   <- either throwPSQLError pure eNews
  let newsJSON = encode $ newsToResponse news
  okResponseWithJSONBody newsJSON

listCommentariesHandler :: Handler
listCommentariesHandler = do
  conn     <- asks hConnection
  req      <- asks hRequest
  dpMap    <- asks hDynamicPathsMap
  maxLimit <- asks hMaxLimit
  let pagination = getLimitOffset maxLimit req
  newsId        <- either throwParseError pure $ getIdFromUrl dpMap
  eCommentaries <- liftIO $ E.try $ selectCommentariesByNewsId conn
                                                               pagination
                                                               newsId
  commentaries <- either throwPSQLError pure eCommentaries
  let responseCommentaries  = commentaryToResponse <$> commentaries
      printableCommentaries = encode responseCommentaries
  okResponseWithJSONBody printableCommentaries

createCommentaryHandler :: Handler
createCommentaryHandler = do
  req   <- asks hRequest
  conn  <- asks hConnection
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let createCommentaryData = eitherDecode body
  newsId <- either throwParseError pure $ getIdFromUrl dpMap
  either (throwError . ParseError)
         (createCommentary conn newsId)
         createCommentaryData
 where
  createCommentary conn newsId commentaryData = do
    eCommentary <- liftIO $ withPSQLException $ insertCommentary
      conn
      newsId
      (requestToCommentary commentaryData)
    commentary <- either (throwError . PSQLError) pure $ join eCommentary
    let commentaryJSON = encode $ commentaryToResponse commentary
    okResponseWithJSONBody commentaryJSON

searchNews :: Handler
searchNews = do
  conn     <- asks hConnection
  req      <- asks hRequest
  maxLimit <- asks hMaxLimit
  dpMap    <- asks hDynamicPathsMap
  let pagination = getLimitOffset maxLimit req
      sorter     = getSorter req
  searchText <- either throwParseError pure $ getSearchTextFromUrl dpMap
  eNews      <- liftIO $ withPSQLException $ findNewsNested conn
                                                            pagination
                                                            searchText
                                                            sorter
  news <- either (throwError . PSQLError) pure eNews
  let responseNews  = newsToResponse <$> news
      printableNews = encode responseNews
  okResponseWithJSONBody printableNews

listNews :: Handler
listNews = do
  maxLimit <- asks hMaxLimit
  conn     <- asks hConnection
  req      <- asks hRequest
  let pagination = getLimitOffset maxLimit req
      filter     = getFilter req
      sorter     = getSorter req
  eNews <- liftIO $ withPSQLException $ selectNewsNested conn
                                                         pagination
                                                         filter
                                                         sorter
  news <- either (throwError . PSQLError) pure eNews
  let responseNews  = newsToResponse <$> news
      printableNews = encode responseNews
  okResponseWithJSONBody printableNews
