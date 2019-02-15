{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module MockMonad where

import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified WebServer.HandlerMonad        as HM
import qualified WebServer.HandlerClass        as HC
import qualified WebServer.MonadDatabase       as MD
import qualified WebServer.Database            as D
import qualified Database.PostgreSQL.Simple    as PSQL
import           Network.Wai
import           Database.Models.Author
import           Database.Models.User
import           Data.Proxy
import           Debug.Trace
import qualified Data.Map.Strict               as M
import           Data.Time
import qualified Data.ByteString.Lazy.Char8    as BC

data MockIO = MockIO {
  mockDB :: MockDB,
  mockReqBody :: BC.ByteString
} deriving Show

type MockHandler = MockMonad Response

newtype MockMonad a = MockMonad {
  runMockMonad :: StateT MockIO (ReaderT HM.HandlerEnv (Except HM.HandlerError)) a
} deriving (Functor, Applicative, Monad, MonadReader HM.HandlerEnv, MonadError HM.HandlerError, MonadState MockIO)


runMock
  :: MockDB
  -> BC.ByteString
  -> D.Limit
  -> HM.DynamicPathsMap
  -> Request
  -> PSQL.Connection
  -> MockMonad a
  -> Either HM.HandlerError a
runMock db body maxLimit dpMap req conn =
  runExcept . (`runReaderT` r) . (`evalStateT` e) . runMockMonad
 where
  r = HM.HandlerEnv { hMaxLimit        = maxLimit
                    , hDynamicPathsMap = dpMap
                    , hRequest         = req
                    , hConnection      = conn
                    }
  e = MockIO { mockDB = db, mockReqBody = body }

data MockDB = DB (M.Map Integer User) (M.Map Integer Author) deriving Show

class Unwraped a where
  unwrap :: MockDB -> M.Map Integer a
  select :: (D.Limit, D.Offset) -> MockDB -> [a]
  select (D.Limit limit, D.Offset offset) db = take l $ drop o xs
    where
      l = fromIntegral limit
      o = fromIntegral offset
      xs = M.elems $ unwrap db
  selectById :: Integer -> MockDB -> Maybe a
  selectById id db = M.lookup id $ unwrap db

instance Unwraped User where
  unwrap (DB us _) = us

instance MD.PersistentUser MockMonad where
  selectUsers p = Right . select p <$> gets mockDB
  selectUserById id = do
    mdb <- gets mockDB
    let mbUser = selectById id mdb
    pure $ maybe (Left "No such id") Right mbUser
  deleteUserById _ = pure $ Right ()
  insertUser UserRaw {..} = pure $ Right User { userId          = 1
                                              , userName        = userRawName
                                              , userSurname     = userRawSurname
                                              , userAvatar      = userRawAvatar
                                              , userDateCreated = sometime
                                              , userIsAdmin     = False
                                              }

sometime :: LocalTime
sometime = LocalTime
  { localDay       = ModifiedJulianDay { toModifiedJulianDay = 162342 }
  , localTimeOfDay = TimeOfDay { todHour = 4, todMin = 8, todSec = 15 }
  }

instance HC.MonadHTTP MockMonad where
  getRequestBody _ = gets mockReqBody
  respond s h b = pure $ responseLBS s h b
