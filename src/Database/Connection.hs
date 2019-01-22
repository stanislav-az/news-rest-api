module Database.Connection where

import           Database.PostgreSQL.Simple

connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost     = "" --omitting the host parameter will cause libpq to attempt to connect via unix domain sockets 
                          , connectPort     = 5432
                          , connectUser     = "stanislav"
                          , connectPassword = "" --connecting via unix sockets tends to use the peer authentication method, which is very secure and does not require a password
                          , connectDatabase = "tesths"
                          }