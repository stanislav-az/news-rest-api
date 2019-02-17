{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module WebServer.Error where

import qualified Control.Exception             as E
                                                ( SomeException(..)
                                                , try
                                                )
import qualified Control.Monad.Except          as E
                                                ( MonadError(..) )
import qualified Network.Wai                   as W
                                                ( Response(..) )
import           WebServer.HandlerMonad         ( HandlerError(..)
                                                , unprocessableEntityResponse
                                                , notFoundResponse
                                                , badRequestResponse
                                                )
import           WebServer.HandlerClass         ( MonadLogger(..)
                                                , MonadHTTP(..)
                                                )
import           Helpers                        ( texify )

manageHandlerError
  :: (MonadHTTP m, MonadLogger m) => HandlerError -> m W.Response
manageHandlerError e@(ParseError _) = logWarn (texify e) >> badRequestResponse
manageHandlerError e@Forbidden =
  logWarn
      "(Authorization failed)/(Attempted removing admin user or default entity)"
    >> notFoundResponse
manageHandlerError e@(PSQLError _) =
  logError (texify e) >> unprocessableEntityResponse

withPSQLException :: IO a -> IO (Either String a)
withPSQLException io = E.try io >>= either left right
 where
  left :: E.SomeException -> IO (Either String a)
  left  = pure . Left . show
  right = pure . Right

throwParseError :: E.MonadError HandlerError m => String -> m a
throwParseError err =
  E.throwError $ ParseError $ "Could not parse dynamic url: " ++ err

throwPSQLError :: E.MonadError HandlerError m => E.SomeException -> m a
throwPSQLError err = E.throwError $ PSQLError $ show (err :: E.SomeException)
