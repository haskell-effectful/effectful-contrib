-- | PostgreSQL logging back-end.
module Effectful.Log.Backend.PostgreSQL
  ( -- * Logging to a PostgreSQL database
    runPgLogging

    -- * Bindings
  , withPgLogger
  ) where

import Data.Text (Text)
import Effectful.Internal.Monad
import Effectful.Log.Logger (Logger)
import Effectful.Log (Logging)
import qualified Effectful.Log
import Effectful.Monad
import Database.PostgreSQL.PQTypes (ConnectionSourceM)
import Log.Data (LogLevel)
import qualified Log.Backend.PostgreSQL as Log

-- | A handler for the 'Logging' effect using a PostgreSQL database as a
-- backend.
runPgLogging
  :: IOE :> es
  => ConnectionSourceM IO
  -> Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runPgLogging cs component maxLogLevel k = withPgLogger cs $ \logger -> do
  Effectful.Log.runLogging component logger maxLogLevel k

----------------------------------------
-- Bindings

-- | Lifted 'Log.withPgLogger'.
withPgLogger
  :: IOE :> es
  => ConnectionSourceM IO -> (Logger -> Eff es a) -> Eff es a
withPgLogger conf f = unsafeEff $ \es -> do
  Log.withPgLogger conf ((`unEff` es) . f)
