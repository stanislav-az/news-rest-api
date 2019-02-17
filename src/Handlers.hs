{-# LANGUAGE FlexibleContexts #-}

module Handlers where

import qualified Network.Wai                   as W
                                                ( Response(..) )
import qualified Control.Exception             as E
                                                ( try )
import qualified Data.Aeson                    as JSON
                                                ( FromJSON(..)
                                                , ToJSON(..)
                                                , encode
                                                , eitherDecode
                                                )
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad                 as M
                                                ( join )
import qualified Control.Monad.IO.Class        as MIO
                                                ( liftIO )
import           Serializer.Author
import           Serializer.Tag
import           Serializer.Category
import           Serializer.News
import           Serializer.Commentary
import           Database.Queries.Author        ( updateAuthor )
import           Database.Queries.Tag           ( updateTag )
import           Database.Queries.Category      ( updateCategory )
import           Database.Queries.News          ( selectNewsNested
                                                , findNewsNested
                                                , updateNews
                                                , publishNews
                                                )
import           Database.Queries.Commentary    ( insertCommentary
                                                , selectCommentariesByNewsId
                                                )
import           WebServer.HandlerMonad         ( Handler(..)
                                                , HandlerError(..)
                                                , HandlerEnv(..)
                                                , okResponseWithJSONBody
                                                , okResponse
                                                )
import           WebServer.MonadDatabase        ( Inserter(..)
                                                , Selector(..)
                                                , Deleter(..)
                                                )
import           WebServer.HandlerClass         ( MonadHTTP(..) )
import           WebServer.Error                ( throwPSQLError
                                                , throwParseError
                                                , withPSQLException
                                                )
import           WebServer.UrlParser.Pagination ( getLimitOffset )
import           WebServer.UrlParser.Sorter     ( getSorter )
import           WebServer.UrlParser.Filter     ( getFilter )
import           WebServer.UrlParser.Dynamic    ( getIdFromUrl
                                                , getSearchTextFromUrl
                                                )

create
  :: ( MonadReader HandlerEnv m
     , MonadError HandlerError m
     , MonadHTTP m
     , JSON.ToJSON a1
     , JSON.FromJSON t
     )
  => Inserter t1 m a
  -> (t -> t1)
  -> (a -> a1)
  -> m W.Response
create inserter requestToEntity entityToResponse = do
  req  <- asks hRequest
  conn <- asks hConnection
  body <- getRequestBody req
  let createEntityData = JSON.eitherDecode body
  either (throwError . ParseError) (createEntity conn) createEntityData
 where
  createEntity conn entityData = do
    eEntity <- inserter $ requestToEntity entityData
    entity  <- either (throwError . PSQLError) pure eEntity
    let entityJSON = JSON.encode $ entityToResponse entity
    okResponseWithJSONBody entityJSON

list
  :: ( MonadReader HandlerEnv m
     , MonadError HandlerError m
     , MonadHTTP m
     , JSON.ToJSON b
     )
  => Selector m a
  -> (a -> b)
  -> m W.Response
list selector entityToResponse = do
  req      <- asks hRequest
  maxLimit <- asks hMaxLimit
  let pagination = getLimitOffset maxLimit req
  eEntities <- selector pagination
  entities  <- either (throwError . PSQLError) pure eEntities
  let responseEntities  = entityToResponse <$> entities
      printableEntities = JSON.encode responseEntities
  okResponseWithJSONBody printableEntities

remove
  :: (MonadReader HandlerEnv m, MonadError HandlerError m, MonadHTTP m)
  => Deleter m
  -> m W.Response
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
  let updateAuthorData = JSON.eitherDecode body
  authorId <- either throwParseError pure $ getIdFromUrl dpMap
  conn     <- asks hConnection
  either (throwError . ParseError)
         (goUpdateAuthor conn authorId)
         updateAuthorData
 where
  goUpdateAuthor conn authorId authorData = do
    let partial = requestToUpdateAuthor authorData
    eAuthor <- MIO.liftIO $ E.try $ updateAuthor conn authorId partial
    author  <- either throwPSQLError pure eAuthor
    let authorJSON = JSON.encode $ authorToUpdateResponse author
    okResponseWithJSONBody authorJSON

updateTagHandler :: Handler
updateTagHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateTagData = JSON.eitherDecode body
  tagId <- either throwParseError pure $ getIdFromUrl dpMap
  conn  <- asks hConnection
  either (throwError . ParseError) (goUpdateTag conn tagId) updateTagData
 where
  goUpdateTag conn tagId tagData = do
    let partial = requestToUpdateTag tagData
    eTag <- MIO.liftIO $ E.try $ updateTag conn tagId partial
    tag  <- either throwPSQLError pure eTag
    let tagJSON = JSON.encode $ tagToResponse tag
    okResponseWithJSONBody tagJSON

updateCategoryHandler :: Handler
updateCategoryHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateCategoryData = JSON.eitherDecode body
  categoryId <- either throwParseError pure $ getIdFromUrl dpMap
  conn       <- asks hConnection
  either (throwError . ParseError)
         (goUpdateCategory conn categoryId)
         updateCategoryData
 where
  goUpdateCategory conn categoryId categoryData = do
    let partial = requestToUpdateCategory categoryData
    eCategory <- MIO.liftIO $ E.try $ updateCategory conn categoryId partial
    category  <- either throwPSQLError pure eCategory
    let categoryJSON = JSON.encode $ categoryNestedToResponse category
    okResponseWithJSONBody categoryJSON

updateNewsHandler :: Handler
updateNewsHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateNewsData = JSON.eitherDecode body
  newsId <- either throwParseError pure $ getIdFromUrl dpMap
  conn   <- asks hConnection
  either (throwError . ParseError) (goUpdateNews conn newsId) updateNewsData
 where
  goUpdateNews conn newsId newsData = do
    let partial = requestToUpdateNews newsData
    eNews <- MIO.liftIO $ E.try $ updateNews conn newsId partial
    news  <- either throwPSQLError pure eNews
    let newsJSON = JSON.encode $ newsToResponse news
    okResponseWithJSONBody newsJSON

publishNewsHandler :: Handler
publishNewsHandler = do
  req    <- asks hRequest
  dpMap  <- asks hDynamicPathsMap
  body   <- getRequestBody req
  newsId <- either throwParseError pure $ getIdFromUrl dpMap
  conn   <- asks hConnection
  eNews  <- MIO.liftIO $ E.try $ publishNews conn newsId
  news   <- either throwPSQLError pure eNews
  let newsJSON = JSON.encode $ newsToResponse news
  okResponseWithJSONBody newsJSON

listCommentariesHandler :: Handler
listCommentariesHandler = do
  conn     <- asks hConnection
  req      <- asks hRequest
  dpMap    <- asks hDynamicPathsMap
  maxLimit <- asks hMaxLimit
  let pagination = getLimitOffset maxLimit req
  newsId        <- either throwParseError pure $ getIdFromUrl dpMap
  eCommentaries <- MIO.liftIO $ E.try $ selectCommentariesByNewsId conn
                                                                   pagination
                                                                   newsId
  commentaries <- either throwPSQLError pure eCommentaries
  let responseCommentaries  = commentaryToResponse <$> commentaries
      printableCommentaries = JSON.encode responseCommentaries
  okResponseWithJSONBody printableCommentaries

createCommentaryHandler :: Handler
createCommentaryHandler = do
  req   <- asks hRequest
  conn  <- asks hConnection
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let createCommentaryData = JSON.eitherDecode body
  newsId <- either throwParseError pure $ getIdFromUrl dpMap
  either (throwError . ParseError)
         (createCommentary conn newsId)
         createCommentaryData
 where
  createCommentary conn newsId commentaryData = do
    eCommentary <- MIO.liftIO $ withPSQLException $ insertCommentary
      conn
      newsId
      (requestToCommentary commentaryData)
    commentary <- either (throwError . PSQLError) pure $ M.join eCommentary
    let commentaryJSON = JSON.encode $ commentaryToResponse commentary
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
  eNews      <- MIO.liftIO $ withPSQLException $ findNewsNested conn
                                                                pagination
                                                                searchText
                                                                sorter
  news <- either (throwError . PSQLError) pure eNews
  let responseNews  = newsToResponse <$> news
      printableNews = JSON.encode responseNews
  okResponseWithJSONBody printableNews

listNews :: Handler
listNews = do
  maxLimit <- asks hMaxLimit
  conn     <- asks hConnection
  req      <- asks hRequest
  let pagination = getLimitOffset maxLimit req
      filter     = getFilter req
      sorter     = getSorter req
  eNews <- MIO.liftIO $ withPSQLException $ selectNewsNested conn
                                                             pagination
                                                             filter
                                                             sorter
  news <- either (throwError . PSQLError) pure eNews
  let responseNews  = newsToResponse <$> news
      printableNews = JSON.encode responseNews
  okResponseWithJSONBody printableNews
