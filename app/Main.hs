{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Logger.Simple         as L
                                                ( withGlobalLogging )
import qualified Network.Wai.Handler.Warp      as W
                                                ( run )
import qualified Network.Wai                   as W
                                                ( Request(..)
                                                , Response(..)
                                                )
import qualified Database.PostgreSQL.Simple    as PSQL
                                                ( close )
import qualified Control.Monad.Except          as E
                                                ( catchError )
import qualified Control.Exception             as E
                                                ( bracket )
import           WebServer.Application          ( newsServer
                                                , withLogging
                                                )
import           Config                         ( getLogConfig
                                                , loadConfig
                                                , getByName
                                                )
import           WebServer.HandlerClass         ( logError
                                                , logInfo
                                                )
import           Routes                         ( routes )
import           WebServer.HandlerMonad         ( DynamicPathsMap(..)
                                                , Handler(..)
                                                , runHandler
                                                , serverErrorResponse
                                                )
import           Database.Connection            ( connect )
import           WebServer.UrlParser.Pagination ( Limit(..) )
import           Helpers                        ( texify )
import           WebServer.Error                ( manageHandlerError )

main :: IO ()
main = getLogConfig >>= \logConf -> L.withGlobalLogging logConf $ do
  logInfo "Starting server at: http://localhost:8080/"
  W.run 8080 (withLogging $ newsServer routes runHAndCatchE)

runHAndCatchE :: W.Request -> DynamicPathsMap -> Handler -> IO W.Response
runHAndCatchE req dpMap handler = do
  conf     <- loadConfig
  maxLimit <- Limit <$> getByName conf "pagination.max_limit"
  res      <- E.bracket (connect conf) PSQL.close $ \conn ->
    runHandler maxLimit dpMap req conn $ E.catchError handler manageHandlerError
  either (\e -> (logError $ texify e) >> serverErrorResponse) pure res
