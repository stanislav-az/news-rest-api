{-# LANGUAGE OverloadedStrings #-}

module Main where

import News.Config (getByName, getLogConfig, loadConfig)
import qualified Control.Exception as E (bracket)
import qualified Service.Logger as L (withGlobalLogging)
import qualified Control.Monad.Except as E (catchError)
import News.Database.Connection (connect)
import qualified Database.PostgreSQL.Simple as PSQL (close)
import Ext.Data.Text (textify)
import qualified Network.Wai as W (Request(..), Response(..))
import qualified Network.Wai.Handler.Warp as W (run)
import News.Routes (routes)
import News.WebServer.Application (newsServer, withLogging)
import News.WebServer.Error (manageHandlerError)
import News.WebServer.HandlerClass (logError, logInfo)
import News.WebServer.HandlerMonad
  ( DynamicPathsMap(..)
  , Handler(..)
  , runHandler
  , serverErrorResponse
  )
import News.WebServer.UrlParser.Pagination (Limit(..))

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
  either (\e -> (logError $ textify e) >> serverErrorResponse) pure res
