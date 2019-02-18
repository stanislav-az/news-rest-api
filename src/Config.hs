{-# LANGUAGE OverloadedStrings #-}

module Config
  ( loadConfig
  , getByName
  , getMaybe
  , getLogConfig
  , C.Config
  )
where

import qualified Data.Configurator             as C
                                                ( lookup
                                                , autoReload
                                                , autoConfig
                                                , require
                                                )
import qualified Data.Configurator.Types       as C
                                                ( Worth(..)
                                                , Config(..)
                                                , Configured(..)
                                                , Name(..)
                                                )
import qualified Control.Logger.Simple         as L
                                                ( LogConfig(..) )
import qualified System.FilePath.Posix         as D
                                                ( takeDirectory )
import qualified System.Directory              as D
                                                ( createDirectoryIfMissing )

loadConfig :: IO C.Config
loadConfig =
  fst <$> C.autoReload C.autoConfig [C.Required "./conf/configuration.local"]

getLogConfig :: IO L.LogConfig
getLogConfig = do
  conf        <- loadConfig
  logToStdout <- getByName conf "logging.log_to_stdout"
  logToFile   <- getMaybe conf "logging.log_to_file"
  let logDir = D.takeDirectory <$> logToFile
  maybe (pure ()) (D.createDirectoryIfMissing True) logDir
  pure $ L.LogConfig { L.lc_file = logToFile, L.lc_stderr = logToStdout }

getByName :: C.Configured a => C.Config -> C.Name -> IO a
getByName = C.require

getMaybe :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
getMaybe = C.lookup


