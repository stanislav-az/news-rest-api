{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Service.Logger.LogConfig where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data LogConfig = LogConfig
  { logToStdErr :: Bool,
    logToFile :: Maybe FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
