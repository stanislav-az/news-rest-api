module Database.Migration where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           System.Directory               ( createDirectoryIfMissing )
import           Control.Exception              ( bracket )
import qualified Database.Connection           as DC
import qualified Config                        as C

initializeDB :: IO ()
initializeDB = do
  conf <- C.loadConfig
  createDirectoryIfMissing False "./DBMigrations"
  bracket (DC.connect conf) close migrate

migrate :: Connection -> IO ()
migrate conn = do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> error err
    _                  -> return ()
  where cmds = [MigrationInitialization, MigrationDirectory "./DBMigrations"]
