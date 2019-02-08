{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module WebServer.HandlerMonad where

import qualified Control.Exception             as E
import qualified Config                        as C
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Data.ByteString.Lazy.Char8    as BC
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Network.Wai
import           WebServer.HandlerClass

-- dynamic path information type and value pairs
type DynamicPathsMap = [(T.Text, T.Text)]

type Handler = HandlerMonad Response

data HandlerEnv = HandlerEnv {
    hConfig :: C.Config,
    hDynamicPathsMap :: DynamicPathsMap,
    hRequest :: Request,
    hConnection :: PSQL.Connection
}

data HandlerError = PSQLError String | ParseError String | Forbidden deriving (Show)

newtype HandlerMonad a = HandlerMonad {runHandlerMonad :: ReaderT HandlerEnv (ExceptT HandlerError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerEnv, MonadError HandlerError)

runHandler
  :: C.Config
  -> DynamicPathsMap
  -> Request
  -> PSQL.Connection
  -> HandlerMonad a
  -> IO (Either HandlerError a)
runHandler conf dpMap req conn =
  runExceptT . (`runReaderT` env) . runHandlerMonad
 where
  env = HandlerEnv { hConfig          = conf
                   , hDynamicPathsMap = dpMap
                   , hRequest         = req
                   , hConnection      = conn
                   }

instance MonadDatabase HandlerMonad where
  type Gateway HandlerMonad = PSQL.Connection
  select c p = liftIO $ select c p
  selectById c i = liftIO $ selectById c i
  delete p c i = liftIO $ delete p c i
  insert c o = liftIO $ insert c o

instance MonadHTTP HandlerMonad where
  getRequestBody = liftIO . getRequestBody
  respond s h b = liftIO $ respond s h b

instance MonadLogger HandlerMonad where
  logDebug = liftIO . logDebug
  logInfo  = liftIO . logInfo
  logWarn  = liftIO . logWarn
  logError = liftIO . logError

notFoundResponse :: (MonadHTTP m) => m Response
notFoundResponse = respond HTTP.status404 [] ""

okResponse :: (MonadHTTP m) => m Response
okResponse = respond HTTP.status204 [] ""

okResponseWithJSONBody :: (MonadHTTP m) => BC.ByteString -> m Response
okResponseWithJSONBody = respond HTTP.status200 []

serverErrorResponse :: (MonadHTTP m) => m Response
serverErrorResponse = respond HTTP.status500 [] ""

badRequestResponse :: (MonadHTTP m) => m Response
badRequestResponse = respond HTTP.status400 [] ""

-- The request was well-formed but was unable to be followed due to semantic errors.
unprocessableEntityResponse :: (MonadHTTP m) => m Response
unprocessableEntityResponse = respond HTTP.status422 [] ""
