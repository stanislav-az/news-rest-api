{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
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
  userDateCreated :: LocalTime,
  userIsAdmin :: Bool
} deriving Show

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

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
} deriving Show

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

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

getAuthorsList :: IO [(User, Author)]
getAuthorsList = bracket (connect connectInfo) close $ \conn ->
  fmap inductiveTupleToTuple
    <$> (query_ conn authorsQuery :: IO [User :. Author])
 where
  authorsQuery
    = "SELECT  u.*, a.*  FROM authors AS a \
    \INNER JOIN users AS u \
    \ON u.user_id = a.user_id"

inductiveTupleToTuple (u :. a) = (u, a)

addAuthorToDB :: (UserRaw, AuthorRaw) -> IO (User, Author)
addAuthorToDB (UserRaw {..}, AuthorRaw {..}) =
  bracket (connect connectInfo) close $ \conn -> do
    (user : _) <- query conn
                        insertUserQuery
                        (userRawName, userRawSurname, userRawAvatar)
    (author : _) <- query conn
                          insertAuthorQuery
                          (userId user, authorRawDescription)
    pure (user, author)
--execute conn insertUserQuery [userRawName, userRawSurname, userRawAvatar]

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING user_id, name, surname, avatar, date_created, is_admin"

insertAuthorQuery :: Query
insertAuthorQuery =
  "INSERT INTO authors(author_id, user_id, description) VALUES (default,?,?) RETURNING author_id, user_id, description"
