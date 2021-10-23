{-# LANGUAGE OverloadedStrings#-}

-- | Bulk stdout logging back-end.
module Effectful.Log.Backend.StandardOutput.Bulk
  ( -- * Bulk logging to stdout
    runBulkStdOutLogging
  , runBulkJsonStdOutLogging

    -- * Bindings
  , withBulkStdOutLogger
  , withBulkJsonStdOutLogger
  ) where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Lazy.Char8 as BSL
import Effectful.Monad
import Log (LogLevel)
import qualified Log as LogBase
import System.IO (hFlush, stdout)

import Effectful.Log
import Effectful.Log.Logger

-- | A handler for the 'Logging' effect that is logging to stdout once per
-- second.
--
-- Implemented using 'LogBase.withBulkStdOutLogger'.
runBulkStdOutLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runBulkStdOutLogging component maxLogLevel k =
  withBulkStdOutLogger $ \logger -> do
    runLogging component logger maxLogLevel k

-- | A handler for the 'Logging' effect that is logging to stdout in the JSON
-- format once per second.
--
-- Implemented using 'LogBase.withBulkJsonStdOutLogger'.
runBulkJsonStdOutLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runBulkJsonStdOutLogging component maxLogLevel k =
  withBulkJsonStdOutLogger $ \logger -> do
    runLogging component logger maxLogLevel k

----------------------------------------
-- Bindings

-- | Lifted 'LogBase.withBulkStdOutLogger'.
withBulkStdOutLogger :: IOE :> es => (Logger -> Eff es a) -> Eff es a
withBulkStdOutLogger act = do
  logger <- mkBulkLogger "stdout-bulk" exec cleanup
  withLogger logger act
    where
      exec msgs = liftIO $ do
        mapM_ (Text.putStrLn . LogBase.showLogMessage Nothing) msgs
        hFlush stdout

      cleanup = return ()

-- | Lifted 'LogBase.withBulkJsonStdOutLogger'.
withBulkJsonStdOutLogger :: IOE :> es => (Logger -> Eff es a) -> Eff es a
withBulkJsonStdOutLogger act = do
  logger <- mkBulkLogger "stdout-bulk-json" exec cleanup
  withLogger logger act
  where
    exec msgs = liftIO $ do
        mapM_ (BSL.putStrLn . JSON.encode) msgs
        hFlush stdout

    cleanup = return ()
