{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebServer.HandlerMonad where

import Control.Monad.Except
import qualified Control.Monad.IO.Class as MIO (MonadIO(..), liftIO)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as B8 (ByteString(..))
import qualified Data.Text as T (Text(..))
import qualified Database.PostgreSQL.Simple as PSQL (Connection(..))
import qualified Network.HTTP.Types as HTTP
  ( status200
  , status204
  , status400
  , status404
  , status422
  , status500
  )
import qualified Network.Wai as W (Request(..), Response(..))
import WebServer.HandlerClass (MonadHTTP(..), MonadLogger(..))
import WebServer.MonadDatabase
  ( Authorization(..)
  , PersistentAuthor(..)
  , PersistentCategory(..)
  , PersistentCommentary(..)
  , PersistentNews(..)
  , PersistentTag(..)
  , PersistentUser(..)
  )
import WebServer.UrlParser.Pagination (Limit(..))

-- dynamic path information type and value pairs
type DynamicPathsMap = [(T.Text, T.Text)]

type Handler = HandlerMonad W.Response

data HandlerEnv = HandlerEnv
  { hMaxLimit :: Limit
  , hDynamicPathsMap :: DynamicPathsMap
  , hRequest :: W.Request
  , hConnection :: PSQL.Connection
  }

data HandlerError
  = PSQLError String
  | ParseError String
  | Forbidden
  deriving (Show)

newtype HandlerMonad a = HandlerMonad
  { runHandlerMonad :: ReaderT HandlerEnv (ExceptT HandlerError IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MIO.MonadIO
             , MonadReader HandlerEnv
             , MonadError HandlerError
             )

runHandler ::
     Limit
  -> DynamicPathsMap
  -> W.Request
  -> PSQL.Connection
  -> HandlerMonad a
  -> IO (Either HandlerError a)
runHandler mLimit dpMap req conn =
  runExceptT . (`runReaderT` env) . runHandlerMonad
  where
    env =
      HandlerEnv
        { hMaxLimit = mLimit
        , hDynamicPathsMap = dpMap
        , hRequest = req
        , hConnection = conn
        }

instance Authorization HandlerMonad where
  isAuthorOfNews u i = MIO.liftIO $ isAuthorOfNews u i
  isAuthorOfCommentary u i = MIO.liftIO $ isAuthorOfCommentary u i

instance PersistentUser HandlerMonad where
  selectUsers = MIO.liftIO . selectUsers
  selectUserById = MIO.liftIO . selectUserById
  deleteUserById = MIO.liftIO . deleteUserById
  insertUser = MIO.liftIO . insertUser

instance MonadHTTP HandlerMonad where
  getRequestBody = MIO.liftIO . getRequestBody
  respond s h b = MIO.liftIO $ respond s h b

instance MonadLogger HandlerMonad where
  logDebug = MIO.liftIO . logDebug
  logInfo = MIO.liftIO . logInfo
  logWarn = MIO.liftIO . logWarn
  logError = MIO.liftIO . logError

notFoundResponse :: (MonadHTTP m) => m W.Response
notFoundResponse = respond HTTP.status404 [] ""

okResponse :: (MonadHTTP m) => m W.Response
okResponse = respond HTTP.status204 [] ""

okResponseWithJSONBody :: (MonadHTTP m) => B8.ByteString -> m W.Response
okResponseWithJSONBody =
  respond HTTP.status200 [("Content-Type", "application/json")]

serverErrorResponse :: (MonadHTTP m) => m W.Response
serverErrorResponse = respond HTTP.status500 [] ""

badRequestResponse :: (MonadHTTP m) => m W.Response
badRequestResponse = respond HTTP.status400 [] ""

-- The request was well-formed but was unable to be followed due to semantic errors.
unprocessableEntityResponse :: (MonadHTTP m) => m W.Response
unprocessableEntityResponse = respond HTTP.status422 [] ""

instance PersistentAuthor HandlerMonad where
  selectAuthors = MIO.liftIO . selectAuthors
  deleteAuthorById = MIO.liftIO . deleteAuthorById
  insertAuthor = MIO.liftIO . insertAuthor

instance PersistentTag HandlerMonad where
  selectTags = MIO.liftIO . selectTags
  deleteTagById = MIO.liftIO . deleteTagById
  insertTag = MIO.liftIO . insertTag

instance PersistentCategory HandlerMonad where
  selectCategoriesNested = MIO.liftIO . selectCategoriesNested
  deleteCategoryById = MIO.liftIO . deleteCategoryById
  insertCategory = MIO.liftIO . insertCategory

instance PersistentNews HandlerMonad where
  deleteNewsById = MIO.liftIO . deleteNewsById
  insertNews = MIO.liftIO . insertNews

instance PersistentCommentary HandlerMonad where
  deleteCommentaryById = MIO.liftIO . deleteCommentaryById
