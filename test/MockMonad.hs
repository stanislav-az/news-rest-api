{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Database.Models.Tag
import           Database.Models.Category
import           Database.Models.Commentary
import           Data.Proxy
import           Debug.Trace
import qualified Data.Map.Strict               as M
import           Data.Time
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T

data MockIO = MockIO {
  mockDB :: MockDB,
  mockReqBody :: BC.ByteString,
  mockLog :: [MockLog]
} deriving Show

type MockHandler = MockMonad Response

newtype MockMonad a = MockMonad {
  runMockMonad :: StateT MockIO (ReaderT HM.HandlerEnv (Except HM.HandlerError)) a
} deriving (Functor, Applicative, Monad, MonadReader HM.HandlerEnv, MonadError HM.HandlerError, MonadState MockIO)

data MockLog = Debug T.Text | Info T.Text | Warn T.Text | Error T.Text
  deriving Show

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
  e = MockIO { mockDB = db, mockReqBody = body, mockLog = [] }

instance HC.MonadHTTP MockMonad where
  getRequestBody _ = gets mockReqBody
  respond s h b = pure $ responseLBS s h b

instance HC.MonadLogger MockMonad where
  logDebug e = modify $ \s@MockIO {..} -> s { mockLog = Debug e : mockLog }
  logInfo e = modify $ \s@MockIO {..} -> s { mockLog = Info e : mockLog }
  logWarn e = modify $ \s@MockIO {..} -> s { mockLog = Warn e : mockLog }
  logError e = modify $ \s@MockIO {..} -> s { mockLog = Error e : mockLog }

instance MD.Authorization MockMonad where
  isAuthorOfNews = error "No tests for this authorization type"
  isAuthorOfCommentary user commentId = do
    db <- gets mockDB 
    let id1 = userId user
        id2 = commentaryUserId <$> selectById commentId db
    pure $ either (const False) (== id1) id2

data MockDB = DB (M.Map Integer User) (M.Map Integer (Author, User)) (M.Map Integer Tag) (M.Map Integer CategoryNested) (M.Map Integer Commentary)
  deriving Show

class Unwraped a where
  unwrap :: MockDB -> M.Map Integer a
  select :: (D.Limit, D.Offset) -> MockDB -> [a]
  select (D.Limit limit, D.Offset offset) db = take l $ drop o xs
    where
      l = fromIntegral limit
      o = fromIntegral offset
      xs = M.elems $ unwrap db
  selectById :: Integer -> MockDB -> Either String a
  selectById id db = maybe (Left "No such id") Right $ M.lookup id $ unwrap db
  deleteById :: Proxy a -> Integer -> MockDB -> Either String ()
  deleteById _ id db = if id `elem` keys then Right () else Left "No such id"
      where
        keys = M.keys (unwrap db :: M.Map Integer a)

instance Unwraped User where
  unwrap (DB us _ _ _ _) = us

instance MD.PersistentUser MockMonad where
  selectUsers p = Right . select p <$> gets mockDB
  selectUserById id = selectById id <$> gets mockDB
  deleteUserById id = deleteById (Proxy :: Proxy User) id <$> gets mockDB
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

instance Unwraped (Author, User) where
  unwrap (DB _ as _ _ _) = as

instance MD.PersistentAuthor MockMonad where
  selectAuthors p = Right . select p <$> gets mockDB
  deleteAuthorById id =
    deleteById (Proxy :: Proxy (Author, User)) id <$> gets mockDB
  insertAuthor (AuthorRaw {..}, UserRaw {..}) = pure $ Right (author, user)
   where
    author = Author 1 1 authorRawDescription
    user   = User { userId          = 1
                  , userName        = userRawName
                  , userSurname     = userRawSurname
                  , userAvatar      = userRawAvatar
                  , userDateCreated = sometime
                  , userIsAdmin     = False
                  }

instance Unwraped Tag where
  unwrap (DB _ _ ts _ _) = ts

instance MD.PersistentTag MockMonad where
  selectTags p = Right . select p <$> gets mockDB
  deleteTagById id = deleteById (Proxy :: Proxy Tag) id <$> gets mockDB
  insertTag TagRaw {..} = pure $ Right Tag { tagId = 1, tagName = tagRawName }

instance Unwraped CategoryNested where
  unwrap (DB _ _ _ cs _) = cs

instance MD.PersistentCategory MockMonad where
  selectCategoriesNested p = Right . select p <$> gets mockDB
  deleteCategoryById id =
    deleteById (Proxy :: Proxy CategoryNested) id <$> gets mockDB
  insertCategory CategoryRaw {..} = do
    db <- gets mockDB
    let eParent =
          maybe (Left "no parent") (`selectById` db) categoryRawParentId
    pure $ Right $ either (const root)
                          (CategoryNested 2 categoryRawName)
                          eParent
    where root = Parent 1 categoryRawName

instance Unwraped Commentary where
  unwrap (DB _ _ _ _ cs) = cs

instance MD.PersistentCommentary MockMonad where
  deleteCommentaryById id =
    deleteById (Proxy :: Proxy Commentary) id <$> gets mockDB
