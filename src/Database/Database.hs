module Database.Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           System.Directory               ( createDirectoryIfMissing )
import           Control.Exception (bracket)
import Database.Connection
import Database.Models.User

initializeDB :: IO ()
initializeDB = do
  createDirectoryIfMissing False "./DBMigrations"
  bracket (connect connectInfo) close migrate

migrate :: Connection -> IO ()
migrate conn = do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> error err
    _                  -> return ()
  where cmds = [MigrationInitialization, MigrationDirectory "./DBMigrations"]
