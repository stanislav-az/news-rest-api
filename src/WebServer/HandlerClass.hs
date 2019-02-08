{-# LANGUAGE TypeFamilies #-}

module WebServer.HandlerClass where

import qualified WebServer.Database            as D
import           Data.Proxy
import           Control.Monad.IO.Class
import qualified Network.Wai                   as W
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Network.HTTP.Types            as HTTP
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import qualified Control.Logger.Simple         as L
import qualified Control.Exception             as E

class (MonadIO m) => MonadDatabase m where
  type Gateway m
  select :: (D.Persistent a) => Gateway m -> (D.Limit, D.Offset) -> m (Either String [a])
  selectById :: (D.Persistent a) => Gateway m -> Integer -> m (Either String a)
  delete :: (D.Persistent a) => Proxy a -> Gateway m -> Integer -> m (Either String ())
  insert :: (D.Fit o a) => Gateway m -> o -> m (Either String a)

instance MonadDatabase IO where
  type Gateway IO = PSQL.Connection
  select c p = E.try (D.select c p) >>= either left right
  selectById c i = E.try (D.selectById c i)
    >>= either left (maybeRight "Could not select entity by id")
  delete p c i = E.try (D.delete p c i) >>= either left convertBool
   where
    convertBool b | b         = pure $ Right ()
                  | otherwise = pure $ Left "Could not delete entity"
  insert c o = E.try (D.insert c o)
    >>= either left (maybeRight "Could not insert object")

left :: E.SomeException -> IO (Either String a)
left = pure . Left . show

right :: a -> IO (Either String a)
right = pure . Right

maybeRight :: String -> Maybe a -> IO (Either String a)
maybeRight s = pure . maybe (Left s) Right

class (MonadIO m) => MonadHTTP m where
  getRequestBody :: W.Request -> m BC.ByteString
  respond :: HTTP.Status -> HTTP.ResponseHeaders -> BC.ByteString -> m W.Response

instance MonadHTTP IO where
  getRequestBody = W.strictRequestBody
  respond s h b = pure $ W.responseLBS s h b

class (MonadIO m) => MonadLogger m where
  logDebug :: T.Text -> m ()
  logInfo  :: T.Text -> m ()
  logWarn  :: T.Text -> m ()
  logError :: T.Text -> m ()

instance MonadLogger IO where
  logDebug = L.logDebug
  logInfo  = L.logInfo
  logWarn  = L.logWarn
  logError = L.logError
