module WebServer.MonadDatabase where

import qualified WebServer.Database            as D
import qualified Control.Exception             as E
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           Database.Models.User
import           Database.Models.Author
import           Database.Models.Tag
import           Database.Models.Category
import           Database.Models.News
import           Database.Models.Commentary
import qualified Database.Queries.News         as DQN
import qualified Database.Queries.Commentary   as DQC
import           Data.Proxy

type Selector m a = (D.Limit, D.Offset) -> m (Either String [a])
type Deleter m = Integer -> m (Either String ())
type Inserter b m a = b -> m (Either String a)

class (Monad m) => Authorization m where
  isAuthorOfNews :: User -> Integer -> m Bool
  isAuthorOfCommentary :: User -> Integer -> m Bool

instance Authorization IO where
  isAuthorOfNews u i = withPSQLConnection $ \c -> DQN.isAuthorOfNews c u i
  isAuthorOfCommentary u i =
    withPSQLConnection $ \c -> DQC.isAuthorOfCommentary c u i

class (Monad m) => PersistentUser m where
  selectUsers :: (D.Limit, D.Offset) -> m (Either String [User])
  selectUserById :: Integer -> m (Either String User)
  deleteUserById :: Integer -> m (Either String ())
  insertUser :: UserRaw -> m (Either String User)

instance PersistentUser IO where
  selectUsers p =
    withPSQLConnection $ \c -> E.try (D.select c p) >>= either left right
  selectUserById i = withPSQLConnection $ \c ->
    E.try (D.selectById c i)
      >>= either left (maybeRight "Could not select entity by id")
  deleteUserById i = withPSQLConnection $ \c ->
    E.try (D.delete (Proxy :: Proxy User) c i) >>= either left convertBool
  insertUser o = withPSQLConnection $ \c -> E.try (D.insert c o)
    >>= either left (maybeRight "Could not insert object")

left :: E.SomeException -> IO (Either String a)
left = pure . Left . show

right :: a -> IO (Either String a)
right = pure . Right

maybeRight :: String -> Maybe a -> IO (Either String a)
maybeRight s = pure . maybe (Left s) Right

convertBool :: Bool -> IO (Either String ())
convertBool b | b         = pure $ Right ()
              | otherwise = pure $ Left "Could not delete entity"

withPSQLConnection :: (PSQL.Connection -> IO a) -> IO a
withPSQLConnection = E.bracket (C.loadConfig >>= DC.connect) PSQL.close

class (Monad m) => PersistentAuthor m where
  selectAuthors :: Selector m (Author, User)
  deleteAuthorById :: Deleter m
  insertAuthor :: Inserter (AuthorRaw, UserRaw) m (Author, User)

instance PersistentAuthor IO where
  selectAuthors p =
    withPSQLConnection $ \c -> E.try (D.select c p) >>= either left right
  deleteAuthorById i = withPSQLConnection $ \c ->
    E.try (D.delete (Proxy :: Proxy Author) c i) >>= either left convertBool
  insertAuthor o = withPSQLConnection $ \c -> E.try (D.insert c o)
    >>= either left (maybeRight "Could not insert object")

class (Monad m) => PersistentTag m where
  selectTags :: Selector m Tag
  deleteTagById :: Deleter m
  insertTag :: Inserter TagRaw m Tag

instance PersistentTag IO where
  selectTags p =
    withPSQLConnection $ \c -> E.try (D.select c p) >>= either left right
  deleteTagById i = withPSQLConnection $ \c ->
    E.try (D.delete (Proxy :: Proxy Tag) c i) >>= either left convertBool
  insertTag o = withPSQLConnection $ \c -> E.try (D.insert c o)
    >>= either left (maybeRight "Could not insert object")

class (Monad m) => PersistentCategory m where
  selectCategoriesNested :: Selector m CategoryNested
  deleteCategoryById :: Deleter m
  insertCategory :: Inserter CategoryRaw m CategoryNested

instance PersistentCategory IO where
  selectCategoriesNested p =
    withPSQLConnection $ \c -> E.try (D.select c p) >>= either left right
  deleteCategoryById i =
    withPSQLConnection
      $ \c ->
          E.try (D.delete (Proxy :: Proxy Category) c i)
            >>= either left convertBool
  insertCategory o = withPSQLConnection $ \c -> E.try (D.insert c o)
    >>= either left (maybeRight "Could not insert object")

class (Monad m) => PersistentNews m where
  deleteNewsById :: Deleter m
  insertNews :: Inserter NewsRaw m NewsNested

instance PersistentNews IO where
  deleteNewsById i = withPSQLConnection $ \c ->
    E.try (D.delete (Proxy :: Proxy News) c i) >>= either left convertBool
  insertNews o = withPSQLConnection $ \c -> E.try (D.insert c o)
    >>= either left (maybeRight "Could not insert object")

class (Monad m) => PersistentCommentary m where
  deleteCommentaryById :: Deleter m

instance PersistentCommentary IO where
  deleteCommentaryById i = withPSQLConnection $ \c ->
    E.try (D.delete (Proxy :: Proxy Commentary) c i) >>= either left convertBool
