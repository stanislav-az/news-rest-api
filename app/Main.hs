{-# LANGUAGE OverloadedStrings #-}

module Main where

import           WebServer.Application
import           Database.Migration
import qualified Config                        as C
import qualified Control.Logger.Simple         as L
import           WebServer.HandlerClass
import           Network.Wai.Handler.Warp       ( run )
import           Routes
import           WebServer.HandlerMonad
import           Network.Wai
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           WebServer.UrlParser.Pagination
import           Control.Monad.Except
import           Control.Exception              ( bracket )
import           Helpers
import           WebServer.Error

main :: IO ()
main = C.getLogConfig >>= \logConf -> L.withGlobalLogging logConf $ do
  logInfo "Starting server at: http://localhost:8080/"
  run 8080 (withLogging $ newsServer routes runHAndCatchE)

runHAndCatchE :: Request -> DynamicPathsMap -> Handler -> IO Response
runHAndCatchE req dpMap handler = do
  conf     <- C.loadConfig
  maxLimit <- Limit <$> C.get conf "pagination.max_limit"
  res      <- bracket (DC.connect conf) PSQL.close $ \conn ->
    runHandler maxLimit dpMap req conn $ catchError handler manageHandlerError
  either (\e -> (logError $ texify e) >> serverErrorResponse) pure res
