-- | LogList logging back-end.
module Effectful.Log.Backend.LogList
  ( -- * Logging to lists
    runLogListLogging

    -- * Bindings
  , newLogList
  , getLogList
  , putLogList
  , clearLogList
  , withLogListLogger

    -- Re-exports
  , LogBase.LogList
  ) where

import Data.Text (Text)
import Effectful.Dispatch.Static
import Effectful.Monad
import Log (LogLevel, LogMessage)
import Log.Backend.LogList (LogList)
import qualified Log.Backend.LogList as LogBase

import Effectful.Log
import Effectful.Log.Logger

-- | A handler for the 'Logging' effect that is logging to a 'LogList'.
--
-- Implemented using 'LogBase.withLogListLogger'.
runLogListLogging
  :: IOE :> es
  => LogList
  -> Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runLogListLogging ll component maxLogLevel k =
  withLogListLogger ll $ \logger -> do
    runLogging component logger maxLogLevel k

----------------------------------------
-- Bindings

-- | Lifted 'LogBase.newLogList'.
newLogList :: IOE :> es => Eff es LogList
newLogList = unsafeEff_ LogBase.newLogList

-- | Lifted 'LogBase.getLogList'.
getLogList :: IOE :> es => LogList -> Eff es [LogMessage]
getLogList = unsafeEff_ . LogBase.getLogList

-- | Lifted 'LogBase.putLogList'.
putLogList :: IOE :> es => LogList -> LogMessage -> Eff es ()
putLogList ll = unsafeEff_ . LogBase.putLogList ll

-- | Lifted 'LogBase.clearLogList'
clearLogList :: IOE :> es => LogList -> Eff es ()
clearLogList = unsafeEff_ . LogBase.clearLogList

-- | Lifted 'LogBase.withLogListLogger'.
withLogListLogger :: IOE :> es => LogList -> (Logger -> Eff es a) -> Eff es a
withLogListLogger ll f = unsafeEff $ \es -> do
  LogBase.withLogListLogger ll ((`unEff` es) . f)
