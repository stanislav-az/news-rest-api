{-# LANGUAGE OverloadedStrings #-}
module Database.Connection where

import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Config                        as C

connectInfo :: C.Config -> IO PSQL.ConnectInfo
connectInfo conf = do
  host     <- C.get conf "database.host"
  port     <- C.get conf "database.port"
  user     <- C.get conf "database.user"
  password <- C.get conf "database.password"
  database <- C.get conf "database.database"
  pure $ PSQL.ConnectInfo { PSQL.connectHost     = host
                          , PSQL.connectPort     = port
                          , PSQL.connectUser     = user
                          , PSQL.connectPassword = password
                          , PSQL.connectDatabase = database
                          }

connect :: C.Config -> IO PSQL.Connection
connect conf = connectInfo conf >>= PSQL.connect
