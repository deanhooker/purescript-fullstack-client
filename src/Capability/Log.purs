module Capability.Log where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.DateTime (DateTime)
import Halogen (HalogenM)

data LogLevel = Debug | Info | Warning | Error

type LogEntry =
  { level :: LogLevel
  , message :: String
  , timestamp :: DateTime}

class Monad m <= Log m where
  logEntry :: LogLevel -> String -> m LogEntry
  log :: LogEntry -> m Unit

instance logHalogenM
         :: Log m
         => Log (HalogenM state action slots output m)
         where
  logEntry level = lift <<< logEntry level
  log = lift <<< log
