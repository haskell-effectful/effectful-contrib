module Main where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Effectful (MonadIO (liftIO), runEff)
import Effectful.Colog
import Effectful.FileSystem.IO
import Effectful.Temporary
import System.FilePath
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Actions" testActions

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
