{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Migration
import           System.Directory               ( createDirectoryIfMissing )
import           Data.Text                     as T
                                         hiding ( head )
import           Control.Exception
import           Data.Monoid                    ( (<>) )
import           Data.Time
import           GHC.Int

data User = User {
  userId :: Integer,
  userName :: T.Text,
  userSurname :: T.Text,
  userAvatar :: T.Text,
  userDateCreated :: UTCTime,
  userIsAdmin :: Bool
}

instance ToRow User where
  toRow User {..} =
    [ toField userId
    , toField userName
    , toField userSurname
    , toField userAvatar
    , toField userDateCreated
    , toField userIsAdmin
    ]

data Author = Author {
  authorId :: Integer,
  authorUserId :: Integer,
  authorDescription :: T.Text
}

data AuthorRaw = AuthorRaw {
  authorRawDescription :: T.Text
}

data UserRaw = UserRaw {
  userRawName :: T.Text,
  userRawSurname :: T.Text,
  userRawAvatar :: T.Text
}

initializeDB :: IO ()
initializeDB = do
  createDirectoryIfMissing False "./DBMigrations"
  bracket (connect connectInfo) close migrate

connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost     = "" --omitting the host parameter will cause libpq to attempt to connect via unix domain sockets 
                          , connectPort     = 5432
                          , connectUser     = "stanislav"
                          , connectPassword = "" --connecting via unix sockets tends to use the peer authentication method, which is very secure and does not require a password
                          , connectDatabase = "tesths"
                          }

migrate :: Connection -> IO ()
migrate conn = do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> error err
    _                  -> return ()
  where cmds = [MigrationInitialization, MigrationDirectory "./DBMigrations"]

getList :: FromRow a => Query -> IO [a]
getList tableName = bracket (connect connectInfo) close
  $ \conn -> query_ conn $ "SELECT * FROM " <> tableName

addAuthorToDB :: (UserRaw, AuthorRaw) -> IO Int64
addAuthorToDB (user@UserRaw {..}, author@AuthorRaw {..}) =
  bracket (connect connectInfo) close $ \conn ->
    execute conn insertUserQuery [userRawName, userRawSurname, userRawAvatar]

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,false)"
