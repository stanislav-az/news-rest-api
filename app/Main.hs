{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (getByName, getLogConfig, loadConfig)
import qualified Control.Exception as E (bracket)
import qualified Control.Logger.Simple as L (withGlobalLogging)
import qualified Control.Monad.Except as E (catchError)
import Database.Connection (connect)
import qualified Database.PostgreSQL.Simple as PSQL (close)
import Helpers (texify)
import qualified Network.Wai as W (Request(..), Response(..))
import qualified Network.Wai.Handler.Warp as W (run)
import Routes (routes)
import WebServer.Application (newsServer, withLogging)
import WebServer.Error (manageHandlerError)
import WebServer.HandlerClass (logError, logInfo)
import WebServer.HandlerMonad
  ( DynamicPathsMap(..)
  , Handler(..)
  , runHandler
  , serverErrorResponse
  )
import WebServer.UrlParser.Pagination (Limit(..))

main :: IO ()
main =
  getLogConfig >>= \logConf ->
    L.withGlobalLogging logConf $ do
      logInfo "Starting server at: http://localhost:8080/"
      W.run 8080 (withLogging $ newsServer routes runHAndCatchE)

runHAndCatchE :: W.Request -> DynamicPathsMap -> Handler -> IO W.Response
runHAndCatchE req dpMap handler = do
  conf <- loadConfig
  maxLimit <- Limit <$> getByName conf "pagination.max_limit"
  res <-
    E.bracket (connect conf) PSQL.close $ \conn ->
      runHandler maxLimit dpMap req conn $
      E.catchError handler manageHandlerError
  either (\e -> (logError $ texify e) >> serverErrorResponse) pure res
