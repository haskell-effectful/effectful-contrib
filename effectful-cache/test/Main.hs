{-# LANGUAGE NoOverloadedStrings #-}
module Main where

import qualified Data.Cache as C
import Effectful.Monad
import Prelude hiding (lookup)
import Test.Hspec as H
import qualified Utils as U

import Data.Cache.Effect

main :: IO ()
main = hspec spec

initStringCache :: IO (C.Cache String String)
initStringCache = C.newCache Nothing

initIntCache :: IO (C.Cache Int Int)
initIntCache = C.newCache Nothing

populateIntCache :: (Cache Int Int :> es) => Eff es ()
populateIntCache =
  mapM_ (\(k,v) -> insert @Int @Int k v) [(2,4),(3,6),(4,8),(5,10)]

spec :: Spec
spec = do
    describe "Testing Cache" $ do
      it "Insert & Lookup" $ testInsertAndLookup =<< initIntCache
      it "Listing keys" $ testListKeys =<< initIntCache
      it "Deleting keys" $ testDeleteKeys =<< initIntCache
      it "Filter with key" $ testFilterWithKey =<< initIntCache

---

testInsertAndLookup :: C.Cache Int Int -> Expectation
testInsertAndLookup cache = runEff $ do
  result <- runCacheIO cache insertAndLookup
  result `U.shouldBe` Just 12

insertAndLookup :: (Cache Int Int :> es) => Eff es (Maybe Int)
insertAndLookup = do
  insert @Int @Int 3 12
  lookup @Int 3

testListKeys :: C.Cache Int Int -> Expectation
testListKeys cache = runEff $ do
  result <- runCacheIO cache listKeys
  result `U.shouldBe` [2,3,4,5]

listKeys :: (Cache Int Int :> es) => Eff es [Int]
listKeys = do
  populateIntCache
  keys @Int @Int

testDeleteKeys :: C.Cache Int Int -> Expectation
testDeleteKeys cache = runEff $ do
  result <- runCacheIO cache deleteKeys
  result `U.shouldBe` [2,4]

deleteKeys :: (Cache Int Int :> es) => Eff es [Int]
deleteKeys = do
  populateIntCache
  delete @Int @Int 3
  delete @Int @Int 5
  keys @Int @Int

testFilterWithKey :: C.Cache Int Int -> Expectation
testFilterWithKey cache = runEff $ do
  result <- runCacheIO cache filterKeys
  result `U.shouldBe` [2,4,5]

filterKeys :: (Cache Int Int :> es) => Eff es [Int]
filterKeys = do
  populateIntCache
  filterWithKey @Int @Int (\k _ -> k /= 3)
  keys @Int @Int -- [2,4,5]
