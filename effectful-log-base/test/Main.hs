{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import Effectful.Monad
import Effectful.Time
import Log
import Test.Hspec hiding (shouldBe)

import Effectful.Log
import Effectful.Log.Backend.LogList
import Utils

import StdoutExample ()

default (Text)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonadLog functions" $ do
    it "logMessage" testLogMessage
    it "localData" testLocalData
    it "localDomain" testLocalDomain
    it "localMaxLogLevel" testLocalMaxLogLevel

testLogMessage :: Expectation
testLogMessage = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    logMessage LogTrace "Message 2" Null
    logMessage LogInfo "Message 3" Null
  msgs `shouldBe`
    [ message1
    , message3
    ]

testLocalData :: Expectation
testLocalData = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    localData [ "key" .= "value" ] $ do
      logMessage LogInfo "Message 2" Null
    logMessage LogInfo "Message 3" Null
  msgs `shouldBe`
    [ message1
    , message2
      { lmData = object
        [ "__data_null" .= Null
        , "key" .= "value"
        ]
      }
    , message3
    ]

testLocalDomain :: Expectation
testLocalDomain = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    localDomain "local domain" $ do
      logMessage LogInfo "Message 2" Null
    logMessage LogInfo "Message 3" Null
  msgs `shouldBe`
    [ message1
    , message2
      { lmDomain =
        [ "local domain"
        ]
      }
    , message3
    ]

testLocalMaxLogLevel :: Expectation
testLocalMaxLogLevel = runEff $ do
  msgs <- logTestWith "main" LogInfo $ do
    logMessage LogInfo "Message 1" Null
    localMaxLogLevel LogAttention $ do
      logMessage LogInfo "Message 2" Null
    logMessage LogInfo "Message 3" Null
  msgs `shouldBe`
    [ message1
    , message3
    ]

----------------------------------------
-- Helpers

logTestWith
  :: IOE :> es
  => Text
  -> LogLevel
  -> Eff (Logging : Time : es) ()
  -> Eff es [LogMessage]
logTestWith component logLevel k = do
  ll <- newLogList
  withLogListLogger ll $ \logger -> do
    runCurrentTimePure epoch . runLogging component logger logLevel $ k
  getLogList ll

epoch :: UTCTime
epoch = read "1970-01-01 00:00:00 UTC"

message :: LogMessage
message = LogMessage
  { lmComponent = "main"
  , lmDomain = []
  , lmTime = epoch
  , lmLevel = LogInfo
  , lmMessage = "Message"
  , lmData = object [ "__data_null" .= Null ]
  }

message1 :: LogMessage
message1 = message
  { lmMessage = "Message 1"
  }

message2 :: LogMessage
message2 = message
  { lmMessage = "Message 2"
  }

message3 :: LogMessage
message3 = message
  { lmMessage = "Message 3"
  }
