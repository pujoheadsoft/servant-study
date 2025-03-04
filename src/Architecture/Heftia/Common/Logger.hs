{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Architecture.Heftia.Common.Logger (Logger(..), logInfo, logWarn, logError, runLogger) where

import Control.Monad.Hefty (makeEffectF, type (:!!), type (~>), type (<|), interpret)
import Control.Monad.Logger (runStdoutLoggingT, logErrorN, logWarnN, logInfoN)
import Data.Text (pack)

data Logger a where
  LogInfo :: Show a => a -> Logger ()
  LogWarn :: Show a => a -> Logger ()
  LogError :: Show a => a -> Logger ()
makeEffectF [ ''Logger ]

runLogger :: IO <| ef => eh :!! Logger ': ef ~> eh :!! ef
runLogger = interpret \case
  LogInfo a -> runStdoutLoggingT $ logInfoN $ pack $ show a
  LogWarn a -> runStdoutLoggingT $ logWarnN $ pack $ show a
  LogError a -> runStdoutLoggingT $ logErrorN $ pack $ show a
