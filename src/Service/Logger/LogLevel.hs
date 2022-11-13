{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Service.Logger.LogLevel where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data LogLevel = Debug | Info | Warning | Error
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
