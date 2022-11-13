module News.Database.Migration where

import News.Config (loadConfig)
import qualified Control.Exception as E (bracket)
import News.Database.Connection (connect)
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
import Ext.Data.Text (textify)
import qualified System.Directory as D (createDirectoryIfMissing)
import News.WebServer.HandlerClass (MonadLogger(..))

initializeDB :: IO ()
initializeDB = do
  conf <- loadConfig
  D.createDirectoryIfMissing False "./migrations"
  E.bracket (connect conf) PSQL.close migrate

migrate :: PSQL.Connection -> IO ()
migrate conn = do
  result <- PSQL.withTransaction conn (PSQL.runMigrations False conn cmds)
  case result of
    PSQL.MigrationError err -> logError $ textify err
    _ -> return ()
  where
    cmds =
      [PSQL.MigrationInitialization, PSQL.MigrationDirectory "./migrations"]
