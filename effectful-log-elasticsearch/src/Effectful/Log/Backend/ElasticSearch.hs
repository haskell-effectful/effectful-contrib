-- | Elasticsearch logging back-end.
module Effectful.Log.Backend.ElasticSearch
  ( -- * Logging to an ElasticSearch endpoint
    runElasticSearchLogging

    -- * Bindings
  , checkElasticSearchLogin
  , checkElasticSearchConnection
  , withElasticSearchLogger

    -- Re-exports
  , Log.ElasticSearchConfig
  , Log.defaultElasticSearchConfig
  , Log.esServer
  , Log.esIndex
  , Log.esShardCount
  , Log.esReplicaCount
  , Log.esMapping
  , Log.esLogin
  , Log.esLoginInsecure
  ) where

import Data.Text (Text)
import Effectful.Internal.Monad
import Effectful.Log (Logging, runLogging)
import Effectful.Log.Logger (Logger)
import Effectful.Monad
import Log.Backend.ElasticSearch (ElasticSearchConfig)
import qualified Log.Backend.ElasticSearch as Log
import Log.Data (LogLevel)
import Network.HTTP.Client (HttpException)

-- | A handler for the 'Logging' effect using an ElasticSearch endpoint as a
-- backend.
runElasticSearchLogging
  :: IOE :> es
  => ElasticSearchConfig
  -> Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runElasticSearchLogging conf component maxLogLevel k =
  withElasticSearchLogger conf $ \logger -> do
    runLogging component logger maxLogLevel k

----------------------------------------
-- Bindings

-- | Lifted 'Log.checkElasticSearchLogin'.
checkElasticSearchLogin :: IOE :> es => ElasticSearchConfig -> Eff es ()
checkElasticSearchLogin = unsafeEff_ . Log.checkElasticSearchLogin

-- | Lifted 'Log.checkElasticSearchConnection'.
checkElasticSearchConnection :: IOE :> es => ElasticSearchConfig -> Eff es (Either HttpException ())
checkElasticSearchConnection = unsafeEff_ . Log.checkElasticSearchConnection

-- | Lifted 'Log.withElasticSearchLogger'.
withElasticSearchLogger :: IOE :> es => ElasticSearchConfig -> (Logger -> Eff es a) -> Eff es a
withElasticSearchLogger conf f = unsafeEff $ \es -> do
  Log.withElasticSearchLogger conf ((`unEff` es) . f)
