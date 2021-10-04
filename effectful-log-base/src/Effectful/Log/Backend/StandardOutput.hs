{-# LANGUAGE OverloadedStrings #-}

-- | Stdout logging back-end.
module Effectful.Log.Backend.StandardOutput
  ( withSimpleStdOutLogger
  , withStdOutLogger
  , withJsonStdOutLogger
  ) where

import qualified Data.Text.IO as Text
import Data.Aeson as JSON
import Effectful.Monad
import Effectful.Internal.Monad
import qualified Log as LogBase
import qualified Log.Backend.StandardOutput as LogBase
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Effectful.Log.Logger

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
