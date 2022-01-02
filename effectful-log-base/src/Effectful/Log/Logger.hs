module Effectful.Log.Logger
  ( -- * Creating loggers
    mkLogger
  , mkLogger'
  , mkBulkLogger
  , mkBulkLogger'

    -- * Helper functions from 'Log.Internal.Logger'
  , withLogger

    -- * Re-exports
  , Logger
  ) where

import Data.Text (Text)
import Log (Logger, LogMessage)
import qualified Log as LogBase
import qualified Log.Internal.Logger as LogBase
import Effectful.Dispatch.Static
import Effectful.Monad

-- | Lifted 'LogBase.mkLogger'.
mkLogger :: IOE :> es => Text -> (LogMessage -> Eff es ()) -> Eff es Logger
mkLogger name exec = unsafeEff $ \es -> do
  es' <- cloneEnv es
  LogBase.mkLogger name ((`unEff` es') . exec)

-- | Lifted `LogBase.mkLogger'`.
mkLogger' :: IOE :> es => Int -> Text -> (LogMessage -> Eff es ()) -> Eff es Logger
mkLogger' cap name exec = unsafeEff $ \es -> do
  es' <- cloneEnv es
  LogBase.mkLogger' cap name ((`unEff` es') . exec)

-- | Lifted 'LogBase.mkBulkLogger'.
mkBulkLogger :: IOE :> es => Text -> ([LogMessage] -> Eff es ()) -> Eff es () -> Eff es Logger
mkBulkLogger name exec cleanup = unsafeEff $ \es -> do
  es' <- cloneEnv es
  LogBase.mkBulkLogger name ((`unEff` es') . exec) (unEff cleanup es')

-- | Lifted `LogBase.mkBulkLogger'`.
mkBulkLogger'
  :: IOE :> es
  => Int
  -- ^ queue capacity (default 1000000)
  -> Int
  -- ^ thread delay (microseconds, default 1000000)
  -> Text
  -- ^ logger name
  -> ([LogMessage] -> Eff es ())
  -- ^ write
  -> Eff es ()
  -- ^ flush
  -> Eff es Logger
mkBulkLogger' cap dur name exec cleanup = unsafeEff $ \es -> do
  es' <- cloneEnv es
  LogBase.mkBulkLogger' cap dur name ((`unEff` es') . exec) (unEff cleanup es')

----------------------------------------
-- Helper functions from 'Log.Internal.Logger'

withLogger :: IOE :> es => Logger -> (Logger -> Eff es a) -> Eff es a
withLogger logger f = unsafeEff $ \es -> do
  LogBase.withLogger logger ((`unEff` es) . f)
