module Static (spec) where

import Control.Monad
import Data.ByteString
import Effectful.Monad
import Effectful.Reader
import Test.Hspec as H

import Effectful.Crypto.RNG

spec :: Spec
spec = do
    describe "Static" $ do
      it "Genering random bytes wrapped in a newtype" testRandomBytes
      it "Generating a random number within a range from Reader" testRandomNumber

---

testRandomNumber :: Expectation
testRandomNumber = runEff $ do
  cryptoState <- newCryptoRNGState
  void $ runCryptoRNG cryptoState
            . runReader ((10, 20) :: (Int, Int))
            $ generatingRandomNumber

generatingRandomNumber :: (CryptoRNG :> es, Reader (Int, Int) :> es) => Eff es Int
generatingRandomNumber = do
  bounds <- ask
  randomR bounds

---

testRandomBytes :: Expectation
testRandomBytes = runEff $ do
  cryptoState <- newCryptoRNGState
  void $ runCryptoRNG cryptoState generateUID

newtype UID = UID ByteString
  deriving newtype (Show, Eq, Ord)

generateUID :: (CryptoRNG :> es ) => Eff es UID
generateUID = do
  bytes <- randomBytes 8
  pure $ UID bytes
