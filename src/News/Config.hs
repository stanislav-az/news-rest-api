{-# LANGUAGE OverloadedStrings #-}

module News.Config
  ( loadConfig
  , getByName
  , getMaybe
  , getLogConfig
  , C.Config
  ) where

import qualified Service.Logger as L (LogConfig(..))
import qualified Data.Configurator as C
  ( autoConfig
  , autoReload
  , lookup
  , require
  )
import qualified Data.Configurator.Types as C
  ( Config(..)
  , Configured(..)
  , Name(..)
  , Worth(..)
  )
import qualified System.Directory as D (createDirectoryIfMissing)
import qualified System.FilePath.Posix as D (takeDirectory)

loadConfig :: IO C.Config
loadConfig =
  fst <$> C.autoReload C.autoConfig [C.Required "./conf/configuration.local"]

getLogConfig :: IO L.LogConfig
getLogConfig = do
  conf <- loadConfig
  logToStdout <- getByName conf "logging.log_to_stdout"
  logToFile <- getMaybe conf "logging.log_to_file"
  let logDir = D.takeDirectory <$> logToFile
  maybe (pure ()) (D.createDirectoryIfMissing True) logDir
  pure $ L.LogConfig {L.logToFile = logToFile, L.logToStdErr = logToStdout}

getByName :: C.Configured a => C.Config -> C.Name -> IO a
getByName = C.require

getMaybe :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
getMaybe = C.lookup
