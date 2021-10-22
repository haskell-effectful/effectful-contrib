-- | A logger that produces in-memory 'Text' values. Mainly useful for
-- testing.
module Effectful.Log.Backend.Text
  ( -- * Logging to an in-memory text value
    runSimpleTextLogging

    -- * Bindings
  , withSimpleTextLogger
  ) where

import Data.Text (Text)
import Effectful.Internal.Monad
import Effectful.Monad
import Log (LogLevel)
import qualified Log.Backend.Text as LogBase

import Effectful.Log
import Effectful.Log.Logger

-- | A handler for the 'Logging' effect that is logging to an in-memory 'Text'
-- value.
--
-- Implemented using 'LogBase.withSimpleTextLogger'.
runSimpleTextLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es (Text, a)
runSimpleTextLogging component maxLogLevel k =
  withSimpleTextLogger $ \logger -> do
    runLogging component logger maxLogLevel k

----------------------------------------
-- Bindings

-- | Lifted 'LogBase.withSimpleTextLogger'.
withSimpleTextLogger :: (Logger -> Eff es a) -> Eff es (Text, a)
withSimpleTextLogger act = unsafeEff $ \es -> do
  LogBase.withSimpleTextLogger ((`unEff` es) . act)
