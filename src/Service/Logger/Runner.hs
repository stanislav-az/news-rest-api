{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.Logger.Runner (withGlobalLogging, log, logDebug, logInfo, logWarn, logError) where

import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TQueue as Q
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.IO.Unsafe (unsafePerformIO)
import Service.Logger.LogConfig (LogConfig (..))
import Service.Logger.LogLevel (LogLevel (..))
import Service.Logger.LogMessage (LogMessage (..))
import System.IO (BufferMode (..), Handle, IOMode (AppendMode), hFlush, hSetBuffering, stderr, withFile)
import Prelude hiding (log)

-- TODO do I need to save all logs before exiting on exception?
withGlobalLogging :: LogConfig -> IO a -> IO ()
withGlobalLogging LogConfig {..} io = case logToFile of
  Nothing
    | logToStdErr -> race_ io $ prepareLoggerStd >> loggerStd
    | otherwise -> void io
  Just path -> withFile path AppendMode $ \handle ->
    if logToStdErr
      then race_ io $ prepareLoggerStd >> prepareLoggerFile handle >> logger handle
      else race_ io $ prepareLoggerStd >> loggerFile handle
  where
    prepareLoggerStd = hSetBuffering stderr LineBuffering
    prepareLoggerFile h = hSetBuffering h LineBuffering

loggerFile :: Handle -> IO a
loggerFile handle = forever $ do
  msg <- readLogMessage
  loggerFileCycle handle msg

loggerStd :: IO a
loggerStd = forever $ do
  msg <- readLogMessage
  loggerStdCycle msg

logger :: Handle -> IO a
logger handle = forever $ do
  msg <- readLogMessage
  concurrently_ (loggerStdCycle msg) (loggerFileCycle handle msg)

loggerStdCycle :: BS.ByteString -> IO ()
loggerStdCycle m = BS.hPut stderr m >> hFlush stderr

loggerFileCycle :: Handle -> BS.ByteString -> IO ()
loggerFileCycle handle m = BS.hPut handle m >> hFlush handle

readLogMessage :: IO BS.ByteString
readLogMessage = do
  msg <- atomically $ Q.readTQueue globalQueue
  let bs = LBS.toStrict $ J.encode msg
  pure $ bs `BS.snoc` BS.c2w '\n'

globalQueue :: Q.TQueue LogMessage
globalQueue = unsafePerformIO Q.newTQueueIO
{-# NOINLINE globalQueue #-}

log :: (MonadIO m) => LogLevel -> Text -> m ()
log level message = liftIO $ do
  timestamp <- getCurrentTime
  atomically $ Q.writeTQueue globalQueue LogMessage {..}

logDebug :: (MonadIO m) => Text -> m ()
logDebug = log Debug

logInfo :: (MonadIO m) => Text -> m ()
logInfo = log Info

logWarn :: (MonadIO m) => Text -> m ()
logWarn = log Warning

logError :: (MonadIO m) => Text -> m ()
logError = log Error
