{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebServer.HandlerMonad where

import qualified Config                        as C
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
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

type HandlerException = T.Text

newtype HandlerMonad a = HandlerMonad {runHandlerMonad :: ReaderT HandlerEnv (ExceptT HandlerException IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerEnv, MonadError HandlerException)

runHandler
  :: C.Config
  -> DynamicPathsMap
  -> Request
  -> PSQL.Connection
  -> HandlerMonad a
  -> IO (Either HandlerException a)
runHandler conf dpMap req conn =
  runExceptT . (`runReaderT` env) . runHandlerMonad
 where
  env = HandlerEnv { hConfig          = conf
                   , hDynamicPathsMap = dpMap
                   , hRequest         = req
                   , hConnection      = conn
                   }

instance MonadDatabase HandlerMonad where
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
notFoundResponse =
  respond HTTP.status404 [("Content-Type", "plain/text")] "Not Found"

okResponse :: (MonadHTTP m) => m Response
okResponse = respond HTTP.status204 [] ""

serverErrorResponse :: (MonadHTTP m) => m Response
serverErrorResponse = respond HTTP.status500 [] ""

-- type TestT = ExceptT String (ReaderT String IO) Int
-- let x = pure 1 :: TestT
-- runExceptT x :: ReaderT String IO (Either String Int)
-- (`runReaderT` "lol") $ runExceptT x :: IO (Either String Int)

-- type TestT = ReaderT String (ExceptT String IO) Int
-- let x = pure 1 :: TestT
-- (`runReaderT` "lol") x :: ExceptT String IO Int
-- runExceptT $ (`runReaderT` "lol") x :: IO (Either String Int)
