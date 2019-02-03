{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import           Network.Wai
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Network.HTTP.Types            as HTTP
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import           Data.Aeson
import           Serializer.Author
import           Serializer.Tag
import           Serializer.Category
import           Serializer.News
import           Database.Queries.Author
import           Database.Queries.Tag
import           Database.Queries.Category
import           Database.Queries.News
import           Helpers
import           WebServer.MonadHandler
import           WebServer.Pagination
import           WebServer.Database
import qualified Config                        as C
import           Control.Monad.Reader
import           Data.Maybe                     ( fromMaybe )

getIdFromUrl :: DynamicPathsMap -> Either String Integer
getIdFromUrl dpMap =
  (maybe (Left "no info") Right $ lookup "id" dpMap) >>= textToInteger

create :: (FromJSON a, Fit b c, ToJSON d) => (a -> b) -> (c -> d) -> Handler
create requestToEntity entityToResponse = do
  req  <- asks hRequest
  conn <- asks hConnection
  body <- liftIO $ requestBody req
  let createEntityData = eitherDecode $ LB.fromStrict body
  liftIO $ either (pure . reportParseError) (createEntity conn) createEntityData
 where
  createEntity conn entityData = do
    mbEntity <- insert conn (requestToEntity entityData)
    let entity     = fromMaybe (error "Could not insert entity") mbEntity
        entityJSON = encode $ entityToResponse entity
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       entityJSON

reportParseError :: String -> Response
reportParseError err = responseLBS HTTP.status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> BC.pack err)

list :: (Persistent a, ToJSON b) => (a -> b) -> Handler
list entityToResponse = do
  conn     <- asks hConnection
  conf     <- asks hConfig
  req      <- asks hRequest
  maxLimit <- liftIO $ Limit <$> C.get conf "pagination.max_limit"
  let pagination = getLimitOffset maxLimit req
  entities <- liftIO $ select conn pagination
  let responseEntities  = entityToResponse <$> entities
      printableEntities = encode responseEntities
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     printableEntities

updateAuthorHandler :: Handler
updateAuthorHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- liftIO $ requestBody req
  let updateAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateAuthorRequest
      authorId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError)
                  (goUpdateAuthor conn authorId)
                  updateAuthorData
 where
  goUpdateAuthor
    :: PSQL.Connection -> Integer -> UpdateAuthorRequest -> IO Response
  goUpdateAuthor conn authorId authorData = do
    let partial = requestToUpdateAuthor authorData
    author <- updateAuthor conn authorId partial
    let authorJSON = encode $ authorToUpdateResponse author
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       authorJSON

updateTagHandler :: Handler
updateTagHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- liftIO $ requestBody req
  let updateTagData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateTagRequest
      tagId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO
    $ either (pure . reportParseError) (goUpdateTag conn tagId) updateTagData
 where
  goUpdateTag :: PSQL.Connection -> Integer -> UpdateTagRequest -> IO Response
  goUpdateTag conn tagId tagData = do
    let partial = requestToUpdateTag tagData
    tag <- updateTag conn tagId partial
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       tagJSON

updateCategoryHandler :: Handler
updateCategoryHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- liftIO $ requestBody req
  let
    updateCategoryData =
      eitherDecode $ LB.fromStrict body :: Either String UpdateCategoryRequest
    categoryId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
      $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError)
                  (goUpdateCategory conn categoryId)
                  updateCategoryData
 where
  goUpdateCategory conn categoryId categoryData = do
    let partial = requestToUpdateCategory categoryData
    category <- updateCategory conn categoryId partial
    let categoryJSON = encode $ categoryNestedToResponse category
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       categoryJSON

updateNewsHandler :: Handler
updateNewsHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- liftIO $ requestBody req
  let updateNewsData =
        eitherDecode $ LB.fromStrict body :: Either String UpdateNewsRequest
      newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  liftIO $ either (pure . reportParseError)
                  (goUpdateNews conn newsId)
                  updateNewsData
 where
  goUpdateNews conn newsId newsData = do
    let partial = requestToUpdateNews newsData
    news <- updateNews conn newsId partial
    let newsJSON = encode $ newsToResponse news
    pure $ responseLBS HTTP.status200
                       [("Content-Type", "application/json")]
                       newsJSON

publishNewsHandler :: Handler
publishNewsHandler = do
  req   <- asks hRequest
  dpMap <- asks hDynamicPathsMap
  body  <- liftIO $ requestBody req
  let newsId = either (\e -> error $ "Could not parse dynamic url: " ++ e) id
        $ getIdFromUrl dpMap
  conn <- asks hConnection
  news <- liftIO $ publishNews conn newsId
  let newsJSON = encode $ newsToResponse news
  pure $ responseLBS HTTP.status200
                     [("Content-Type", "application/json")]
                     newsJSON
