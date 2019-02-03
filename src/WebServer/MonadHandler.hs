{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebServer.MonadHandler where

import qualified Config                        as C
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Data.Text                     as T
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import           Network.Wai

-- dynamic path information type and value pairs
type DynamicPathsMap = [(T.Text, T.Text)]

type Handler = MonadHandler Response

data HandlerEnv = HandlerEnv {
    hConfig :: C.Config,
    hDynamicPathsMap :: DynamicPathsMap,
    hRequest :: Request,
    hConnection :: PSQL.Connection
}

newtype MonadHandler a = MonadHandler {runMonadHandler :: ReaderT HandlerEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerEnv)

runHandler
  :: C.Config
  -> DynamicPathsMap
  -> Request
  -> PSQL.Connection
  -> MonadHandler a
  -> IO a
runHandler conf dpMap req conn = (`runReaderT` env) . runMonadHandler
 where
  env = HandlerEnv { hConfig          = conf
                   , hDynamicPathsMap = dpMap
                   , hRequest         = req
                   , hConnection      = conn
                   }
