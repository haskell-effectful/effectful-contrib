{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Effectful.Monad
import Effectful.Temporary
import System.Exit (ExitCode(..))
import System.IO.Error (isDoesNotExistError)
import Test.Hspec as H

import Effectful.Process.Typed

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Simple" $ do

      it "/usr/bin/env true" $ do
        let pc = proc "/usr/bin/env" ["true"]
            action = runEff . runTypedProcess $ do
              p <- startProcess pc
              waitExitCode p
        action `shouldReturn` ExitSuccess

      it "Non-existent binary" $ do
        let pc = proc "/bin/doesnotexist" []
            action = runEff . runTypedProcess $ do
              p <- startProcess pc
              waitExitCode p
        action `shouldThrow` isDoesNotExistError

      it "Continue process execution" $ do
        let pc = shell "sleep 3"
            action = runEff . runTypedProcess $ do
              p <- startProcess pc
              getExitCode p
        action `shouldReturn` Nothing

    describe "Termination" $ do

      it "Terminate process" $ do
        let action = runEff . runTemporary . runTypedProcess $ do
              withSystemTempFile "effectful-typed-process-test" $ \fp h -> do
                let pc = setStdout (useHandleClose h)
                       $ shell "sleep 1; printf 'Output'"
                withProcessTerm pc (const $ pure ())
                liftIO $ readFile fp
        action `shouldReturn` ""

      it "Wait for process" $ do
        let action = runEff . runTemporary . runTypedProcess $ do
              withSystemTempFile "effectful-typed-process-test" $ \fp h -> do
                let pc = setStdout (useHandleClose h)
                       $ shell "sleep 1; printf 'Output'"
                withProcessWait pc (const $ pure ())
                liftIO $ readFile fp
        action `shouldReturn` "Output"

    describe "Helper functions" $ do

      it "runProcess" $ do
        let pc = proc "/usr/bin/env" ["true"]
            action = runEff . runTypedProcess $ do
              runProcess pc
        action `shouldReturn` ExitSuccess

      it "runProcess_" $ do
        let pc = proc "/usr/bin/env" ["true"]
            action = runEff . runTypedProcess $ do
              runProcess_ pc
        action `shouldReturn` ()

      it "readProcess" $ do
        let pc = shell "printf 'stdout'; printf 'stderr' >&2"
            action = runEff . runTypedProcess $ do
              readProcess pc
        action `shouldReturn` (ExitSuccess, "stdout", "stderr")

      it "readProcess_" $ do
        let pc = shell "printf 'stdout'; printf 'stderr' >&2"
            action = runEff . runTypedProcess $ do
              readProcess_ pc
        action `shouldReturn` ("stdout", "stderr")

      it "readProcessStdout" $ do
        let pc = shell "printf 'Output'"
            action = runEff . runTypedProcess $ do
              readProcessStdout pc
        action `shouldReturn` (ExitSuccess, "Output")

      it "readProcessStdout_" $ do
        let pc = shell "printf 'Output'"
            action = runEff . runTypedProcess $ do
              readProcessStdout_ pc
        action `shouldReturn` "Output"

      it "readProcessStderr" $ do
        let pc = shell "printf 'Output' >&2"
            action = runEff . runTypedProcess $ do
              readProcessStderr pc
        action `shouldReturn` (ExitSuccess, "Output")

      it "readProcessStderr_" $ do
        let pc = shell "printf 'Output' >&2"
            action = runEff . runTypedProcess $ do
              readProcessStderr_ pc
        action `shouldReturn` "Output"

    describe "Exit codes" $ do

      it "runProcess_" $ do
        let pc = proc "/usr/bin/env" ["false"]
            action = runEff . runTypedProcess $ do
              runProcess_ pc
        action `shouldThrow` const @_ @ExitCodeException True

      it "readProcess_" $ do
        let pc = proc "/usr/bin/env" ["false"]
            action = runEff . runTypedProcess $ do
              readProcess_ pc
        action `shouldThrow` const @_ @ExitCodeException True

      it "readProcessStdout_" $ do
        let pc = proc "/usr/bin/env" ["false"]
            action = runEff . runTypedProcess $ do
              readProcessStdout_ pc
        action `shouldThrow` const @_ @ExitCodeException True

      it "readProcessStderr_" $ do
        let pc = proc "/usr/bin/env" ["false"]
            action = runEff . runTypedProcess $ do
              readProcessStderr_ pc
        action `shouldThrow` const @_ @ExitCodeException True
