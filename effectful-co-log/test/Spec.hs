module Main where

import Colog (cmap)
import Colog.Core.IO
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Effectful
import Effectful.Colog
import Effectful.Reader (ask, local, runReader)
import qualified Effectful.State.Local as Local
import qualified Effectful.State.Shared as Shared
import System.IO.Silently
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

main :: IO ()
main = hspec $
  describe "Colog" $ do
    describe "runLog" testRunLog
    describe "withLog" testWithLog

testRunLog :: Spec
testRunLog = do
  prop "logs all pure messages" $ \msgs ->
    (runPureEff . execPureLogEff @String) (logMsgs msgs)
      === msgs
  prop "logs all stdout messages" $ \msgs ->
    monadicIO $ do
      stdout <- run $ capture_ $
        (runEff . runLog logStringStdout) (logMsgs msgs)
      pure $ stdout == unlines msgs
  prop "does not alter inner local state" $ \msgs1 msgs2 ->
    let app = do
          Local.modify @Int (+ 1)
          logMsgs msgs1
          Local.modify @Int (+ 1)
          logMsgs msgs2
          Local.modify @Int (+ 1)
     in (runPureEff . runPureLogEff . Local.execState @Int 0) app
          === (3, msgs1 <> msgs2 :: Seq String)
  prop "does not alter outer local state" $ \msgs1 msgs2 ->
    let app = do
          Local.modify @Int (+ 1)
          logMsgs msgs1
          Local.modify @Int (+ 1)
          logMsgs msgs2
          Local.modify @Int (+ 1)
     in (runPureEff . Local.runState @Int 0 . execPureLogEff) app
          === (msgs1 <> msgs2 :: Seq String, 3)
  it "does work with 'Reader' effect" $
    let action = LogAction $ \msg -> do
          prefix <- ask @String
          unLogAction (logMessagePure @String) (prefix <> msg)
        app = do
          logMsg "first"
          local (const "local: ") (logMsg "second")
     in (runPureEff .
          runReader "reader: " . Shared.execState Seq.empty . runLog action) app
          `shouldBe` Seq.fromList ["reader: first", "local: second"]

testWithLog :: Spec
testWithLog = do
  prop "does nothing on id" $ \msgs ->
    let inner = logMsgs @String @[] msgs
    in (runPureEff . runPureLogEff @String . withLog @String id) inner
         === (runPureEff . runPureLogEff) inner
  prop "does modify the LogAction" $ \msgs1 msgs2 f ->
    let app = do
          logMsgs msgs1
          withLog @String (cmap (apply f)) (logMsgs msgs2)
    in (runPureEff . execPureLogEff @String) app
         === msgs1 <> fmap (apply f) msgs2

execPureLogEff :: forall msg a es. Eff (Log msg ': Shared.State (Seq msg) ': es) a -> Eff es (Seq msg)
execPureLogEff = Shared.execState Seq.empty . runLog (logMessagePure @msg)

runPureLogEff :: forall msg a es. Eff (Log msg ': Shared.State (Seq msg) ': es) a -> Eff es (a, Seq msg)
runPureLogEff = Shared.runState Seq.empty . runLog (logMessagePure @msg)
