{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Directory (createDirectoryIfMissing)
import Data.Text as T hiding (head)    

initializeDB :: IO ()
initializeDB = do
    createDirectoryIfMissing False "./DBMigrations"
    conn <- connect connectInfo
    migrate conn
    close conn

connectInfo :: ConnectInfo
connectInfo = ConnectInfo {
    connectHost = "", --omitting the host parameter will cause libpq to attempt to connect via unix domain sockets 
    connectPort = 5432,
    connectUser = "stanislav", 
    connectPassword = "", --connecting via unix sockets tends to use the peer authentication method, which is very secure and does not require a password
    connectDatabase = "hs"}

migrate :: Connection -> IO ()
migrate conn = do
    result <- withTransaction conn (runMigrations False conn cmds)
    case result of
        MigrationError err -> error err
        _ -> return ()
    where
        cmds = [MigrationInitialization, 
                MigrationDirectory "./DBMigrations"]
                