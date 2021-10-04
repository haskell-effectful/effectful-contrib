{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effectful.Log
  ( -- * Logging effect
    Logging
  , runLogging
  , runLoggingWithEnv

    -- * Effectful 'LogBase.MonadLog' methods
  , Effectful.Log.logMessage
  , Effectful.Log.localData
  , Effectful.Log.localDomain
  , Effectful.Log.getLoggerEnv

    -- * Effectful functions of 'LogBase.Monad'
  , logMessageEff
  , getLoggerEff
  ) where

import Data.Aeson.Types (Pair, Value)
import Data.Text (Text)
import Log (Logger, LoggerEnv, LogLevel, MonadLog)
import qualified Log as LogBase
import Data.Time.Clock (UTCTime, getCurrentTime)

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | An effect for structured logging using the @log-base@ library.
data Logging :: Effect where
  Logging :: LoggerEnv -> Logging m r

-- | Run a 'Logging' effect.
--
-- This function is the effectful version of 'LogBase.runLogT'.
runLogging
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> Logger
  -- ^ The logging back-end to use.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runLogging component logger = runLoggingWithEnv LogBase.LoggerEnv
  { LogBase.leLogger = logger
  , LogBase.leComponent = component
  , LogBase.leDomain = []
  , LogBase.leData = []
  }

-- | Run a 'Logging' effect with a given logging environment.
runLoggingWithEnv
  :: IOE :> es
  => LoggerEnv
  -- ^ The logging environment to use.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runLoggingWithEnv logEnv = evalEffect (IdE (Logging logEnv))

----------------------------------------
-- Effectful 'LogBase.MonadLog' methods

-- | A specialized version of 'LogBase.logMessage'.
logMessage :: Logging :> es => LogLevel -> Text -> Value -> Eff es ()
logMessage level message data_ = unsafeEff $ \es -> do
  IdE (Logging logEnv) <- getEnv es
  time <- getCurrentTime
  LogBase.logMessageIO logEnv time level message data_

-- | A specialized version of 'LogBase.localData'.
localData :: Logging :> es => [Pair] -> Eff es a -> Eff es a
localData data_ = localEffect $ \(IdE (Logging logEnv)) -> let
  logEnv' = logEnv { LogBase.leData = data_ ++ LogBase.leData logEnv }
  in IdE (Logging logEnv')

-- | A specialized version of 'LogBase.localDomain'.
localDomain :: Logging :> es => Text -> Eff es a -> Eff es a
localDomain domain = localEffect $ \(IdE (Logging logEnv)) -> let
  logEnv' = logEnv { LogBase.leDomain = LogBase.leDomain logEnv ++ [domain] }
  in IdE (Logging logEnv')

-- | A specialized version of 'LogBase.getLoggerEnv'.
getLoggerEnv :: Logging :> es => Eff es LoggerEnv
getLoggerEnv = do
  IdE (Logging env) <- getEffect
  pure env

----------------------------------------
-- Effectful functions of 'LogBase.Monad'

logMessageEff :: IOE :> es => LoggerEnv -> UTCTime -> LogLevel -> Text -> Value -> Eff es ()
logMessageEff logEnv time level message data_ =
  unsafeEff_ $ LogBase.logMessageIO logEnv time level message data_

getLoggerEff
  :: (IOE :> es', Logging :> es)
  => Eff es (UTCTime -> LogLevel -> Text -> Value -> Eff es' ())
getLoggerEff = logMessageEff <$> Effectful.Log.getLoggerEnv

----------------------------------------
-- Orphan instances for 'Eff'

instance Logging :> es => MonadLog (Eff es) where
  logMessage = Effectful.Log.logMessage

  localData = Effectful.Log.localData

  localDomain = Effectful.Log.localDomain

  getLoggerEnv = Effectful.Log.getLoggerEnv
