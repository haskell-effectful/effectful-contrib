{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effectful.Log
  ( -- * Logging effect
    Logging
  , runLogging
  , runLoggingWithEnv

    -- * Effectful functions of 'LogBase.Monad'
  , logMessageEff
  , getLoggerEff

    -- * 'LogBase.MonadLog' methods specialized to `Eff`
  , logMessageEff'
  , localDataEff'
  , localDomainEff'
  , localMaxLogLevelEff'
  , getLoggerEnvEff'
  ) where

import Data.Aeson.Types (Pair, Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Effectful.Dispatch.Static
import Effectful.Monad
import Effectful.Time (Time, getCurrentTime)
import Log (Logger, LoggerEnv, LogLevel, MonadLog)
import qualified Log as LogBase

-- | An effect for structured logging using the @log-base@ library.
data Logging :: Effect

type instance DispatchOf Logging = 'Static
newtype instance StaticRep Logging = Logging LoggerEnv

-- | Run a 'Logging' effect.
--
-- This function is the effectful version of 'LogBase.runLogT'.
runLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> Logger
  -- ^ The logging back-end to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runLogging component logger maxLogLevel = runLoggingWithEnv LogBase.LoggerEnv
  { LogBase.leLogger = logger
  , LogBase.leComponent = component
  , LogBase.leDomain = []
  , LogBase.leData = []
  , LogBase.leMaxLogLevel = maxLogLevel
  }

-- | Run a 'Logging' effect with a given logging environment.
runLoggingWithEnv
  :: IOE :> es
  => LoggerEnv
  -- ^ The logging environment to use.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runLoggingWithEnv logEnv = evalStaticRep (Logging logEnv)

----------------------------------------
-- Effectful 'LogBase.MonadLog' methods

-- | A specialized version of 'LogBase.logMessage'.
logMessageEff'
  :: (Logging :> es, Time :> es)
  => LogLevel
  -> Text
  -> Value
  -> Eff es ()
logMessageEff' level message data_ = do
  time <- getCurrentTime
  Logging logEnv <- getStaticRep
  unsafeEff_ $ LogBase.logMessageIO logEnv time level message data_

-- | A specialized version of 'LogBase.localData'.
localDataEff' :: Logging :> es => [Pair] -> Eff es a -> Eff es a
localDataEff' data_ = localStaticRep $ \(Logging logEnv) ->
  Logging logEnv { LogBase.leData = data_ ++ LogBase.leData logEnv }

-- | A specialized version of 'LogBase.localDomain'.
localDomainEff' :: Logging :> es => Text -> Eff es a -> Eff es a
localDomainEff' domain = localStaticRep $ \(Logging logEnv) ->
  Logging logEnv { LogBase.leDomain = LogBase.leDomain logEnv ++ [domain] }

-- | A specialized version of 'LogBase.localMaxLogLevel'.
localMaxLogLevelEff' :: Logging :> es => LogLevel -> Eff es a -> Eff es a
localMaxLogLevelEff' level = localStaticRep $ \(Logging logEnv) ->
  Logging logEnv { LogBase.leMaxLogLevel = level }

-- | A specialized version of 'LogBase.getLoggerEnv'.
getLoggerEnvEff' :: Logging :> es => Eff es LoggerEnv
getLoggerEnvEff' = do
  Logging env <- getStaticRep
  pure env

----------------------------------------
-- Effectful functions of 'LogBase.Monad'

logMessageEff :: IOE :> es => LoggerEnv -> UTCTime -> LogLevel -> Text -> Value -> Eff es ()
logMessageEff logEnv time level message data_ =
  unsafeEff_ $ LogBase.logMessageIO logEnv time level message data_

getLoggerEff
  :: (IOE :> es', Logging :> es)
  => Eff es (UTCTime -> LogLevel -> Text -> Value -> Eff es' ())
getLoggerEff = logMessageEff <$> getLoggerEnvEff'

----------------------------------------
-- Orphan instances for 'Eff'

instance (Logging :> es, Time :> es) => MonadLog (Eff es) where
  logMessage = logMessageEff'

  localData = localDataEff'

  localDomain = localDomainEff'

  localMaxLogLevel = localMaxLogLevelEff'

  getLoggerEnv = getLoggerEnvEff'
