-- | LogList logging back-end.
module Effectful.Log.Backend.LogList
  ( -- * Logging to lists
    newLogList
  , getLogList
  , putLogList
  , clearLogList
  , withLogListLogger

    -- Re-exports
  , LogBase.LogList
  ) where

import Effectful.Internal.Monad
import Effectful.Monad
import Log.Data (LogMessage)
import Log.Backend.LogList (LogList)
import qualified Log.Backend.LogList as LogBase

import Effectful.Log.Logger

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
