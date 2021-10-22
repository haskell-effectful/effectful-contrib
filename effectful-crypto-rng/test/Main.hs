module Main (main) where

import Test.Hspec

import qualified Dynamic
import qualified Static

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Testing CryptoRNG" $ do
    Dynamic.spec
    Static.spec
