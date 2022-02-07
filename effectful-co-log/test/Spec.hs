module Main where

import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Effectful
import Effectful.Colog
import Effectful.Concurrent (runConcurrent)
import Effectful.Concurrent.Async (concurrently_, mapConcurrently_)
import Effectful.FileSystem.IO
import qualified Effectful.State.Shared as Shared
import Effectful.Temporary
import System.FilePath
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Actions" testActions
  describe "Concurrent" testConcurrent

testActions :: Spec
testActions = do
  describe "logTextHandle" $
    prop "logs messages to a temporary file handle" $
      forAll (listOf genAlphaNumText) $ \msgs ->
        ioProperty . runEff . runFileSystem . runTemporary . withSystemTempDirectory "co-log-test" $
          \tempDir -> do
            let path = tempDir </> "log.txt"
            withFile path WriteMode $ \handle ->
              runLog (logTextHandle handle) (logMsgs msgs)
            logs <- liftIO $ Text.readFile path
            pure $ logs === Text.unlines msgs

  describe "withLogTextFile" $
    prop "logs messages to a temporary file" $
      forAll (listOf genAlphaNumText) $ \msgs ->
        ioProperty . runEff . runTemporary . withSystemTempDirectory "co-log-test" $
          \tempDir -> do
            let path = tempDir </> "log.txt"
            withLogTextFile path (`runLog` logMsgs msgs)
            logs <- liftIO $ Text.readFile path
            pure $ logs === Text.unlines msgs

-- Only use aphanumeric characters as there are some edge cases with control characters, such as '\r'.
-- This is probably only an issue on Windows.
genAlphaNumText :: Gen Text
genAlphaNumText = Text.pack <$> listOf (suchThat arbitraryASCIIChar isAlphaNum)

testConcurrent :: Spec
testConcurrent = do
  describe "withBackgroundLogger" $ do
    prop "logs all messages" $ \msgs ->
      ioProperty . runEff . runConcurrent $ do
        logs <-
          Shared.execState @(Seq String) Seq.empty $
            withBackgroundLogger
              defCapacity
              (logMessagePure @String)
              (`runLog` logMsgs @String msgs)
        pure $ logs === msgs
    prop "logs all messages concurrently" $ \msgs1 msgs2 ->
      ioProperty . runEff . runConcurrent $ do
        logs <-
          Shared.execState @(Seq (Either String String)) Seq.empty $
            withBackgroundLogger
              defCapacity
              (logMessagePure @(Either String String))
              ( `runLog`
                  concurrently_
                    (logMsgs @(Either String String) $ map Left msgs1)
                    (logMsgs @(Either String String) $ map Right msgs2)
              )
        pure $ partitionEithers (toList logs) === (msgs1, msgs2)

    prop "logs all messages in arbitrary many threads" $ \msgss ->
      ioProperty . runEff . runConcurrent $ do
        logs <-
          Shared.execState @(Seq String) Seq.empty $
            withBackgroundLogger
              defCapacity
              (logMessagePure @String)
              ( `runLog`
                  mapConcurrently_ @[] (logMsgs @String) msgss
              )
        pure $ sort (toList logs) === sort (concat msgss)

  describe "forkBackgroundLogger" $ do
    prop "logs all messages" $ \msgs ->
      ioProperty . runEff . runConcurrent $ do
        logs <- Shared.execState @(Seq String) Seq.empty $ do
          logger <- forkBackgroundLogger defCapacity (logMessagePure @String)
          let action = convertToLogAction logger
          runLog action (logMsgs @String msgs)
        pure $ logs === msgs

    prop "logs all messages concurrently" $ \msgs1 msgs2 ->
      ioProperty . runEff . runConcurrent $ do
        logs <- Shared.execState @(Seq (Either String String)) Seq.empty $ do
          logger <- forkBackgroundLogger defCapacity (logMessagePure @(Either String String))
          let action = convertToLogAction logger
          runLog action $
            concurrently_
              (logMsgs @(Either String String) $ map Left msgs1)
              (logMsgs @(Either String String) $ map Right msgs2)
        pure $ partitionEithers (toList logs) === (msgs1, msgs2)

  describe "mkBackgroundThread" $ do
    prop "logs all messages" $ \msgs ->
      ioProperty . runEff . runConcurrent $ do
        logs <- Shared.execState @(Seq String) Seq.empty $ do
          logger <- mkBackgroundThread defCapacity
          let action = runInBackgroundThread logger (logMessagePure @String)
          runLog action (logMsgs @String msgs)
        pure $ logs === msgs

    prop "logs all messages concurrently" $ \msgs1 msgs2 ->
      ioProperty . runEff . runConcurrent $ do
        logs <- Shared.execState @(Seq (Either String String)) Seq.empty $ do
          logger <- mkBackgroundThread defCapacity
          let action = runInBackgroundThread logger (logMessagePure @(Either String String))
          runLog action $
            concurrently_
              (logMsgs @(Either String String) $ map Left msgs1)
              (logMsgs @(Either String String) $ map Right msgs2)
        pure $ partitionEithers (toList logs) === (msgs1, msgs2)
