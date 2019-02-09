{-# LANGUAGE OverloadedStrings #-}

module Config
  ( loadConfig
  , get
  , getMaybe
  , getLogConfig
  , C.Config
  )
where

import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C
import qualified Control.Logger.Simple         as L

loadConfig :: IO C.Config
loadConfig =
  fst <$> C.autoReload C.autoConfig [C.Required "./conf/configuration.local"]

getLogConfig :: IO L.LogConfig
getLogConfig = do
  conf        <- loadConfig
  logToStdout <- get conf "logging.log_to_stdout"
  logToFile   <- getMaybe conf "logging.log_to_file"
  pure $ L.LogConfig { L.lc_file = logToFile, L.lc_stderr = logToStdout }

get :: C.Configured a => C.Config -> C.Name -> IO a
get = C.require

getMaybe :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
getMaybe = C.lookup


