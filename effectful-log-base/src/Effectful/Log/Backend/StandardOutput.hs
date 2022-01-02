{-# LANGUAGE OverloadedStrings #-}

-- | Stdout logging back-end.
module Effectful.Log.Backend.StandardOutput
  ( -- * Logging to stdout
    runSimpleStdOutLogging
  , runStdOutLogging
  , runJsonStdOutLogging

    -- * Bindings
  , withSimpleStdOutLogger
  , withStdOutLogger
  , withJsonStdOutLogger
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Aeson as JSON
import Effectful.Dispatch.Static
import Effectful.Monad
import Log (LogLevel)
import qualified Log as LogBase
import qualified Log.Backend.StandardOutput as LogBase
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Effectful.Log
import Effectful.Log.Logger

-- | A handler for the 'Logging' effect that is logging to stdout in a simple
-- format.
--
-- Implemented using 'LogBase.withSimpleStdOutLogger'.
runSimpleStdOutLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runSimpleStdOutLogging component maxLogLevel k =
  withSimpleStdOutLogger $ \logger -> do
    runLogging component logger maxLogLevel k

-- | A handler for the 'Logging' effect that is logging to stdout.
--
-- Implemented using 'LogBase.withStdOutLogger'.
runStdOutLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runStdOutLogging component maxLogLevel k = withStdOutLogger $ \logger -> do
  runLogging component logger maxLogLevel k

-- | A handler for the 'Logging' effect that is logging to stdout in the JSON
-- format.
--
-- Implemented using 'LogBase.withJsonStdOutLogger'.
runJsonStdOutLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runJsonStdOutLogging component maxLogLevel k =
  withJsonStdOutLogger $ \logger -> do
    runLogging component logger maxLogLevel k

----------------------------------------
-- Bindings

-- | Lifted 'LogBase.withSimpleStdOutLogger'.
withSimpleStdOutLogger :: IOE :> es => (Logger -> Eff es a) -> Eff es a
withSimpleStdOutLogger act = unsafeEff $ \es -> do
  LogBase.withSimpleStdOutLogger ((`unEff` es) . act)

-- | Lifted 'LogBase.withStdOutLogger'.
withStdOutLogger :: IOE :> es => (Logger -> Eff es a) -> Eff es a
withStdOutLogger act = do
  logger <- mkLogger "stdout" $ \msg -> liftIO $ do
    Text.putStrLn $ LogBase.showLogMessage Nothing msg
    hFlush stdout
  withLogger logger act

-- | Lifted 'LogBase.withJsonStdOutLogger'.
withJsonStdOutLogger :: IOE :> es => (Logger -> Eff es a) -> Eff es a
withJsonStdOutLogger act = do
  logger <- mkLogger "stdout-json" $ \msg -> liftIO $ do
    BSL.putStrLn $ JSON.encode msg
    hFlush stdout
  withLogger logger act
