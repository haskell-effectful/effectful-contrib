-- | Elasticsearch logging back-end.
module Effectful.Log.Backend.ElasticSearch
  ( -- * Logging to lists
    checkElasticSearchLogin
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

import Effectful.Internal.Monad
import Log (Logger)
import Effectful.Monad
import Log.Backend.ElasticSearch (ElasticSearchConfig)
import qualified Log.Backend.ElasticSearch as Log
import Network.HTTP.Client (HttpException)

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
