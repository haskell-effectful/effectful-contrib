{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-|
  Module      : Effectful.Crypto.RNG
  Copyright   : © Hécate Moonlight, 2021
                  Dominik Peteler, 2021
  License     : MIT
  Maintainer  : hecate@glitchbra.in
  Stability   : stable

  An effect wrapper around Crypto.RNG for the Effectful ecosystem
-}
module Effectful.Crypto.RNG
  ( -- * CryptoRNG Effect
    CryptoRNG

    -- * Runner
  , runCryptoRNG

    -- * CryptorRNG functions
  , CryptoRNGState
  , randomString
  , randomBytes
  , randomR
  , newCryptoRNGState
  , unsafeCryptoRNGState

    -- * Re-exports from Crypto.RNG
  , C.mapCryptoRNGT
  , C.runCryptoRNGT
  , C.withCryptoRNGState
  ) where

import Crypto.Classes (ByteLength)
import Crypto.RNG (CryptoRNGState)
import Data.ByteString (ByteString)
import Effectful.Dispatch.Static
import Effectful.Monad
import qualified Crypto.RNG as C
import qualified Crypto.RNG.Utils as C

-- | An effect for the cryptographic random generator provided by the DRBG package.
data CryptoRNG :: Effect

type instance DispatchOf CryptoRNG = 'Static
newtype instance StaticRep CryptoRNG = CryptoRNG CryptoRNGState

-- | The default Effect handler
runCryptoRNG
  :: IOE :> es
  => CryptoRNGState
  -> Eff (CryptoRNG : es) a
  -> Eff es a
runCryptoRNG rngState = evalStaticRep (CryptoRNG rngState)

-- | Create a new 'CryptoRNGState', based on system entropy.
newCryptoRNGState :: IOE :> es => Eff es CryptoRNGState
newCryptoRNGState = C.newCryptoRNGState

-- | Create a new 'CryptoRNGState', based on a bytestring seed.
-- Should only be used for testing.
unsafeCryptoRNGState :: IOE :> es => ByteString -> Eff es CryptoRNGState
unsafeCryptoRNGState seed = C.unsafeCryptoRNGState seed

-- | Generate given number of cryptographically secure random bytes.
randomBytes :: CryptoRNG :> es => ByteLength -> Eff es ByteString
randomBytes len = do
  CryptoRNG rngState <- getStaticRep
  unsafeEff_ $ C.randomBytesIO len rngState

-- | Generate random string of specified length that contains allowed chars.
randomString :: CryptoRNG :> es => Int -> String -> Eff es String
randomString len allowedChars = do
  CryptoRNG rngState <- getStaticRep
  unsafeEff_ $ C.runCryptoRNGT rngState (C.randomString len allowedChars)

-- | Generate a cryptographically secure random number in given, closed range.
randomR :: (CryptoRNG :> es, Integral a) => (a, a) -> Eff es a
randomR (low, high) = do
  CryptoRNG rngState <- getStaticRep
  unsafeEff_ $ C.runCryptoRNGT rngState $ C.randomR (low, high)
