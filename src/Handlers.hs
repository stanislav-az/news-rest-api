{-# LANGUAGE OverloadedStrings #-}
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
import           WebServer.HandlerClass
import           WebServer.Error
import qualified WebServer.Database            as D
import           WebServer.Pagination
import qualified Config                        as C
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Maybe                     ( fromMaybe )

getIdFromUrl :: DynamicPathsMap -> Either String Integer
getIdFromUrl dpMap =
  (maybe (Left "no info") Right $ lookup "id" dpMap) >>= textToInteger

create :: (FromJSON a, D.Fit b c, ToJSON d) => (a -> b) -> (c -> d) -> Handler
create requestToEntity entityToResponse = do
  req  <- asks hRequest
  conn <- asks hConnection
  body <- getRequestBody req
  let createEntityData = eitherDecode body
  either (throwError . ParseError) (createEntity conn) createEntityData
 where
  createEntity conn entityData = do
    eEntity <- insert conn (requestToEntity entityData)
    entity  <- either (throwError . PSQLError) pure eEntity
    let entityJSON = encode $ entityToResponse entity
    respond HTTP.status200 [("Content-Type", "application/json")] entityJSON

reportParseError :: (MonadHTTP m) => String -> m Response
reportParseError err = respond HTTP.status400
                               [("Content-Type", "plain/text")]
                               ("Parse error: " <> BC.pack err)

list :: (D.Persistent a, ToJSON b) => (a -> b) -> Handler
list entityToResponse = do
  conn     <- asks hConnection
  conf     <- asks hConfig
  req      <- asks hRequest
  maxLimit <- liftIO $ Limit <$> C.get conf "pagination.max_limit"
  let pagination = getLimitOffset maxLimit req
  eEntities <- select conn pagination
  entities  <- either (throwError . PSQLError) pure eEntities
  let responseEntities  = entityToResponse <$> entities
      printableEntities = encode responseEntities
  respond HTTP.status200
          [("Content-Type", "application/json")]
          printableEntities

remove :: (D.Persistent e) => Proxy e -> Handler
remove this = do
  dpMap    <- asks hDynamicPathsMap
  conn     <- asks hConnection
  entityId <-
    either
        (\e -> throwError $ ParseError $ "Could not parse dynamic url: " ++ e)
        pure
      $ getIdFromUrl dpMap
  if entityId == 0
    then throwError Forbidden
    else do
      res <- delete this conn entityId
      either (throwError . PSQLError) pure res
      okResponse

updateAuthorHandler :: Handler
updateAuthorHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateAuthorData = eitherDecode body
      authorId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  either reportParseError (goUpdateAuthor conn authorId) updateAuthorData
 where
  goUpdateAuthor conn authorId authorData = do
    let partial = requestToUpdateAuthor authorData
    author <- liftIO $ updateAuthor conn authorId partial
    let authorJSON = encode $ authorToUpdateResponse author
    respond HTTP.status200 [("Content-Type", "application/json")] authorJSON

updateTagHandler :: Handler
updateTagHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateTagData = eitherDecode body :: Either String UpdateTagRequest
      tagId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either reportParseError (goUpdateTag conn tagId) updateTagData
 where
  goUpdateTag :: PSQL.Connection -> Integer -> UpdateTagRequest -> IO Response
  goUpdateTag conn tagId tagData = do
    let partial = requestToUpdateTag tagData
    tag <- updateTag conn tagId partial
    let tagJSON = encode $ tagToResponse tag
    respond HTTP.status200 [("Content-Type", "application/json")] tagJSON

updateCategoryHandler :: Handler
updateCategoryHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let
    updateCategoryData =
      eitherDecode body :: Either String UpdateCategoryRequest
    categoryId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
      $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either reportParseError
                  (goUpdateCategory conn categoryId)
                  updateCategoryData
 where
  goUpdateCategory conn categoryId categoryData = do
    let partial = requestToUpdateCategory categoryData
    category <- updateCategory conn categoryId partial
    let categoryJSON = encode $ categoryNestedToResponse category
    respond HTTP.status200 [("Content-Type", "application/json")] categoryJSON

updateNewsHandler :: Handler
updateNewsHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let updateNewsData = eitherDecode body :: Either String UpdateNewsRequest
      newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either reportParseError (goUpdateNews conn newsId) updateNewsData
 where
  goUpdateNews conn newsId newsData = do
    let partial = requestToUpdateNews newsData
    news <- updateNews conn newsId partial
    let newsJSON = encode $ newsToResponse news
    respond HTTP.status200 [("Content-Type", "application/json")] newsJSON

publishNewsHandler :: Handler
publishNewsHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  news <- liftIO $ publishNews conn newsId
  let newsJSON = encode $ newsToResponse news
  respond HTTP.status200 [("Content-Type", "application/json")] newsJSON

listCommentariesHandler :: Handler
listCommentariesHandler = do
  conn     <- asks hConnection
  conf     <- asks hConfig
  req      <- asks hRequest
  dpMap    <- asks hDynamicPathsMap
  maxLimit <- liftIO $ Limit <$> C.get conf "pagination.max_limit"
  let pagination = getLimitOffset maxLimit req
      newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  commentaries <- liftIO $ selectCommentariesByNewsId conn pagination newsId
  let responseCommentaries  = commentaryToResponse <$> commentaries
      printableCommentaries = encode responseCommentaries
  respond HTTP.status200
          [("Content-Type", "application/json")]
          printableCommentaries

createCommentaryHandler :: Handler
createCommentaryHandler = do
  req   <- asks hRequest
  conn  <- asks hConnection
  dpMap <- asks hDynamicPathsMap
  body  <- getRequestBody req
  let createCommentaryData = eitherDecode body
      newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  liftIO $ either reportParseError
                  (createCommentary conn newsId)
                  createCommentaryData
 where
  createCommentary conn newsId commentaryData = do
    mbCommentary <- insertCommentary conn
                                     newsId
                                     (requestToCommentary commentaryData)
    maybe (putStrLn "No posted news with such id" >> notFoundResponse)
          commentaryPostedResponse
          mbCommentary
  commentaryPostedResponse commentary = do
    let commentaryJSON = encode $ commentaryToResponse commentary
    respond HTTP.status200 [("Content-Type", "application/json")] commentaryJSON
