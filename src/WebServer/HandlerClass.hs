module WebServer.HandlerClass where

import qualified Network.Wai                   as W
                                                ( Request(..)
                                                , Response(..)
                                                , responseLBS
                                                , strictRequestBody
                                                )
import qualified Network.HTTP.Types            as HTTP
                                                ( Status(..)
                                                , ResponseHeaders(..)
                                                )
import qualified Data.ByteString.Lazy.Char8    as BC
                                                ( ByteString(..) )
import qualified Data.Text                     as T
                                                ( Text(..) )
import qualified Control.Logger.Simple         as L
                                                ( logDebug
                                                , logInfo
                                                , logWarn
                                                , logError
                                                )

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
