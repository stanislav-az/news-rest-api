{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module WebServer.Error
  ( texifyE
  , manageHandlerError
  )
where

import           WebServer.HandlerMonad
import           WebServer.HandlerClass
import qualified Data.Text                     as T
import           Network.Wai
import qualified Control.Exception             as E
import           Control.Monad.Except
import           Control.Monad.IO.Class

manageHandlerError :: (MonadHTTP m, MonadLogger m) => HandlerError -> m Response
manageHandlerError e@(ParseError _) = logWarn (texifyE e) >> badRequestResponse
manageHandlerError e@Forbidden =
  logWarn "Attempted removing admin user or default category"
    >> notFoundResponse
manageHandlerError e@(PSQLError _) =
  logError (texifyE e) >> unprocessableEntityResponse

withPSQLException :: IO a -> HandlerMonad a
withPSQLException io = liftIO (E.try io) >>= either rethrow pure
 where
  rethrow :: E.SomeException -> HandlerMonad a
  rethrow = throwError . PSQLError . show

texifyE :: HandlerError -> T.Text
texifyE = T.pack . show
