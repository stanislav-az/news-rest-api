module WebServer.HandlerClass where

import qualified WebServer.Database            as D
import           Data.Proxy
import qualified Network.Wai                   as W
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Network.HTTP.Types            as HTTP
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import qualified Control.Logger.Simple         as L

class (Monad m) => MonadDatabase m where
  select :: (D.Persistent a) => PSQL.Connection -> (D.Limit, D.Offset) -> m [a]
  selectById :: (D.Persistent a) => PSQL.Connection -> Integer -> m (Maybe a)
  delete :: (D.Persistent a) => Proxy a -> PSQL.Connection -> Integer -> m Bool
  insert :: (D.Fit o a) => PSQL.Connection -> o -> m (Maybe a)

instance MonadDatabase IO where
  select     = D.select
  selectById = D.selectById
  delete     = D.delete
  insert     = D.insert

class (Monad m) => MonadHTTP m where
  getRequestBody :: W.Request -> m BC.ByteString
  respond :: HTTP.Status -> HTTP.ResponseHeaders -> BC.ByteString -> m W.Response

instance MonadHTTP IO where
  getRequestBody = W.strictRequestBody
  respond s h b = pure $ W.responseLBS s h b

class (Monad m) => MonadLogger m where
  logDebug :: T.Text -> m ()
  logInfo  :: T.Text -> m ()
  logWarn  :: T.Text -> m ()
  logError :: T.Text -> m ()

instance MonadLogger IO where
  logDebug = L.logDebug
  logInfo  = L.logInfo
  logWarn  = L.logWarn
  logError = L.logError

-- class EnvReader m where == Reader
--   getEnv :: m e

-- class MonadException m where == Except
--   catchE 
--   throwE
