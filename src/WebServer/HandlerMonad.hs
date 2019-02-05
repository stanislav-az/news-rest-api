{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebServer.HandlerMonad where

import qualified Config                        as C
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Data.Text                     as T
import           Control.Monad.Reader
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

newtype HandlerMonad a = HandlerMonad {runHandlerMonad :: ReaderT HandlerEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerEnv)

runHandler
  :: C.Config
  -> DynamicPathsMap
  -> Request
  -> PSQL.Connection
  -> HandlerMonad a
  -> IO a
runHandler conf dpMap req conn = (`runReaderT` env) . runHandlerMonad
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
