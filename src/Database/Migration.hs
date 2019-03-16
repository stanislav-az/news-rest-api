module Database.Migration where

import Config (loadConfig)
import qualified Control.Exception as E (bracket)
import Database.Connection (connect)
import qualified Database.PostgreSQL.Simple as PSQL
  ( Connection(..)
  , close
  , withTransaction
  )
import qualified Database.PostgreSQL.Simple.Migration as PSQL
  ( MigrationCommand(..)
  , MigrationResult(..)
  , runMigrations
  )
import Helpers (texify)
import qualified System.Directory as D (createDirectoryIfMissing)
import WebServer.HandlerClass (MonadLogger(..))

initializeDB :: IO ()
initializeDB = do
  conf <- loadConfig
  D.createDirectoryIfMissing False "./migrations"
  E.bracket (connect conf) PSQL.close migrate

migrate :: PSQL.Connection -> IO ()
migrate conn = do
  result <- PSQL.withTransaction conn (PSQL.runMigrations False conn cmds)
  case result of
    PSQL.MigrationError err -> logError $ texify err
    _ -> return ()
  where
    cmds =
      [PSQL.MigrationInitialization, PSQL.MigrationDirectory "./migrations"]
