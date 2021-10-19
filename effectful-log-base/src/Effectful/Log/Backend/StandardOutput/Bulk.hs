{-# LANGUAGE OverloadedStrings#-}

-- | Bulk stdout logging back-end.
module Effectful.Log.Backend.StandardOutput.Bulk
  ( withBulkStdOutLogger
  , withBulkJsonStdOutLogger
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Lazy.Char8 as BSL
import Effectful.Monad
import qualified Log as LogBase
import System.IO (hFlush, stdout)

import Effectful.Log.Logger

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
