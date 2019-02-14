{-# LANGUAGE TypeFamilies #-}

module WebServer.MonadDatabase where

import qualified WebServer.Database            as D
import qualified Control.Exception             as E
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           Database.Models.User
import           Database.Models.Tag
import           Data.Proxy

type Selector m a = (D.Limit, D.Offset) -> m (Either String [a])
type Deleter m = Integer -> m (Either String ())
type Inserter b m a = b -> m (Either String a)

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
