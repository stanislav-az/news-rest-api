{-# LANGUAGE OverloadedStrings #-}

module Database.Connection where

import qualified Database.PostgreSQL.Simple    as PSQL
                                                ( Connection(..)
                                                , ConnectInfo(..)
                                                , connect
                                                )
import qualified Config                        as C
                                                ( Config(..)
                                                , getByName
                                                )

connectInfo :: C.Config -> IO PSQL.ConnectInfo
connectInfo conf = do
  host     <- C.getByName conf "database.host"
  port     <- C.getByName conf "database.port"
  user     <- C.getByName conf "database.user"
  password <- C.getByName conf "database.password"
  database <- C.getByName conf "database.database"
  pure $ PSQL.ConnectInfo { PSQL.connectHost     = host
                          , PSQL.connectPort     = port
                          , PSQL.connectUser     = user
                          , PSQL.connectPassword = password
                          , PSQL.connectDatabase = database
                          }

connect :: C.Config -> IO PSQL.Connection
connect conf = connectInfo conf >>= PSQL.connect
