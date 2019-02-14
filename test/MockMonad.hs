{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MockMonad where

import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified WebServer.HandlerMonad        as HM
import qualified WebServer.HandlerClass        as HC
import qualified WebServer.MonadDatabase       as MD
import           WebServer.MonadDatabase        ( PersistentUser(..) )
import qualified WebServer.Database            as D
import           Network.Wai
import           Database.Models.Author
import           Database.Models.User
import           Data.Proxy
import           Debug.Trace

data MockIO = MockIO {
  mockUsers :: [User],
  mockAuthors :: [Author]
} deriving Show

newtype MockMonad a = MockMonad {
  runMockMonad :: StateT MockIO (ReaderT HM.HandlerEnv (Except HM.HandlerError)) a
} deriving (Functor, Applicative, Monad, MonadReader HM.HandlerEnv, MonadError HM.HandlerError, MonadState MockIO)

runMock
  :: MockIO
  -> HM.HandlerEnv
  -> MockMonad a
  -> Either HM.HandlerError (a, MockIO)
runMock e r = runExcept . (`runReaderT` r) . (`runStateT` e) . runMockMonad

instance PersistentUser MockMonad where
  selectUsers (D.Limit limit, D.Offset offset) = do
    users <- gets mockUsers
    let l = fromIntegral limit
        o = fromIntegral offset
    pure $ Right $ take l $ drop o users
