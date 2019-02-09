{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module WebServer.Error where

import           WebServer.HandlerMonad
import           WebServer.HandlerClass
import           Helpers
import qualified Data.Text                     as T
import           Network.Wai
import qualified Control.Exception             as E
import           Control.Monad.Except
import           Control.Monad.IO.Class

manageHandlerError :: (MonadHTTP m, MonadLogger m) => HandlerError -> m Response
manageHandlerError e@(ParseError _) = logWarn (texify e) >> badRequestResponse
manageHandlerError e@Forbidden =
  logWarn "Attempted removing admin user or default category"
    >> notFoundResponse
manageHandlerError e@(PSQLError _) =
  logError (texify e) >> unprocessableEntityResponse

withPSQLException :: IO a -> IO (Either String a)
withPSQLException io = E.try io >>= either left right
 where
  left :: E.SomeException -> IO (Either String a)
  left  = pure . Left . show
  right = pure . Right

throwParseError :: MonadError HandlerError m => String -> m a
throwParseError err =
  throwError $ ParseError $ "Could not parse dynamic url: " ++ err

throwPSQLError :: MonadError HandlerError m => E.SomeException -> m a
throwPSQLError err = throwError $ PSQLError $ show (err :: E.SomeException)
