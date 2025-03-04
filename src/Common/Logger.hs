module Common.Logger
  ( logInfo,
    logWarn,
    logError,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (logErrorN, logInfoN, logWarnN, runStdoutLoggingT)
import Data.Text (pack)

logInfo :: (MonadIO m, Show a) => a -> m ()
logInfo e = runStdoutLoggingT $ logInfoN $ pack $ show e

logWarn :: (MonadIO m, Show a) => a -> m ()
logWarn e = runStdoutLoggingT $ logWarnN $ pack $ show e

logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ pack $ show e
