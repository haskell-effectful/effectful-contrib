module Main where

import Data.Bifunctor (second)
import Effectful (Eff, runEff, runPureEff, type (:>))
import Effectful.Colog.Core
  ( Log,
    LogAction (..),
    LogActionEff,
    cmap,
    logMsg,
    logMsgs,
    logStringStdout,
    runLog,
    withLog,
  )
import Effectful.Reader.Static (ask, local, runReader)
import qualified Effectful.State.Static.Local as Local
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (idempotentIOProperty, (===))
import Test.QuickCheck.Function (apply)

main :: IO ()
main = hspec $
  describe "Colog" $ do
    describe "runLog" testRunLog
    describe "withLog" testWithLog

testRunLog :: Spec
testRunLog = do
  prop "logs all pure messages" $ \msgs ->
    (runPureEff . execPureLogEff) (logMsgs msgs)
      === msgs

  prop "logs all stdout messages" $ \msgs ->
    idempotentIOProperty $ do
      stdout <- (capture_ . runEff . runLog logStringStdout) (logMsgs msgs)
      pure $ stdout === unlines msgs

  prop "does not alter inner local state" $ \msgs1 msgs2 ->
    let app = do
          Local.modify @Int (+ 1)
          logMsgs msgs1
          Local.modify @Int (+ 1)
          logMsgs msgs2
          Local.modify @Int (+ 1)
     in (runPureEff . runPureLogEff . Local.execState @Int 0) app
          === (3, msgs1 <> msgs2)

  prop "does not alter outer local state" $ \msgs1 msgs2 ->
    let app = do
          Local.modify @Int (+ 1)
          logMsgs msgs1
          Local.modify @Int (+ 1)
          logMsgs msgs2
          Local.modify @Int (+ 1)
     in (runPureEff . Local.runState @Int 0 . execPureLogEff) app
          === (msgs1 <> msgs2, 3)
  it "does work with 'Reader' effect" $
    let action = LogAction $ \msg -> do
          prefix <- ask @String
          Local.modify (<> [prefix <> msg])
        app = do
          logMsg "first"
          local (const "local: ") (logMsg "second")
     in ( runPureEff
            . runReader "reader: "
            . Local.execState []
            . runLog action
        )
          app
          `shouldBe` ["reader: first", "local: second"]

testWithLog :: Spec
testWithLog = do
  prop "does nothing on id" $ \msgs ->
    let inner = logMsgs @String @[] msgs
     in (runPureEff . runPureLogEff . withLog @String id) inner
          === (runPureEff . runPureLogEff) inner

  prop "does modify the LogAction" $ \msgs1 msgs2 f ->
    let app = do
          logMsgs msgs1
          withLog @String (cmap (apply f)) (logMsgs msgs2)
     in (runPureEff . execPureLogEff) app
          === msgs1 <> fmap (apply f) msgs2

pureLogAction :: Local.State [String] :> es => LogActionEff es String
pureLogAction = LogAction $ \msg -> Local.modify (msg :)

runPureLogEff :: Eff (Log String ': Local.State [String] ': es) a -> Eff es (a, [String])
runPureLogEff = fmap (second reverse) . Local.runState [] . runLog pureLogAction

execPureLogEff :: Eff (Log String ': Local.State [String] ': es) a -> Eff es [String]
execPureLogEff = fmap reverse . Local.execState [] . runLog pureLogAction