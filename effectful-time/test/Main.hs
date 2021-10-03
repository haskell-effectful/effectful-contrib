module Main where

import qualified Data.Time as T
import Debug.Trace
import Effectful.Monad
import Effectful.State.Local
import Test.Hspec as H
import qualified Utils as U

import Effectful.Time

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Combining State & Time" $ do
      it "IO Time handler & State" $ testIOTimeAndState =<< T.getCurrentTime
      it "Pure Time handler & State" testPureTimeAndState

---

testIOTimeAndState :: UTCTime -> Expectation
testIOTimeAndState firstTime = runEff $ do
  traceShowM firstTime
  result <- evalState firstTime -- The order in which thes two functions
            . runCurrentTimeIO  -- are composed does not matter. Swap them to try.
            $ storingTimeInState
  result `U.shouldBe` firstTime

storingTimeInState :: (Time :> es, State UTCTime :> es) => Eff es UTCTime
storingTimeInState = do
  firstTime <- get
  secondTime <- action
  if secondTime <= firstTime
  then put secondTime
  else put firstTime
  get

action :: (Time :> es) => Eff es UTCTime
action = do
  T.addUTCTime 100 <$> getCurrentTime

---

testPureTimeAndState :: Expectation
testPureTimeAndState = runEff $ do
  let time = read "2021-07-11 13:30:20 UTC" :: UTCTime
  result <- runCurrentTimePure time
            . evalState time
            $ usingStaticTime
  result `U.shouldBe` True

usingStaticTime :: (Time :> es, State UTCTime :> es) => Eff es Bool
usingStaticTime = do
  t <- getCurrentTime
  t' <- get
  pure $ t == t'
