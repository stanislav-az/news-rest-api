module Database.Migration where

import qualified Database.PostgreSQL.Simple    as PSQL
                                                ( Connection(..)
                                                , withTransaction
                                                , close
                                                )
import qualified Database.PostgreSQL.Simple.Migration
                                               as PSQL
                                                ( MigrationResult(..)
                                                , MigrationCommand(..)
                                                , runMigrations
                                                )
import qualified System.Directory              as D
                                                ( createDirectoryIfMissing )
import qualified Control.Exception             as E
                                                ( bracket )
import           Database.Connection            ( connect )
import           Config                         ( loadConfig )
import           WebServer.HandlerClass         ( MonadLogger(..) )
import           Helpers                        ( texify )

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
    _                       -> return ()
 where
  cmds = [PSQL.MigrationInitialization, PSQL.MigrationDirectory "./migrations"]
