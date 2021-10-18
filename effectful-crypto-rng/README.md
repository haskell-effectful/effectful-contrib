# effectful-crypto-rng

## Description

A `CryptoRNG` effect for the [`effectful`][effectful] ecosystem.

## How to use

This library exposes the following elements:

* `CryptoRNG` — The type-level effect that you can declare in your type signatures.

example:
```haskell
generateUID :: (CryptoRNG :> es) => Eff es UID
```

* `randomR`, `randomBytes`, `randomString`
Functions to get random data from the system's PRNG.

```haskell

newtype UID = UID ByteString
  deriving newtype (Show, Eq, Ord)

import Effectful.Crypto.RNG

generateUID :: (CryptoRNG :> es ) => Eff es UID
generateUID = do
  bytes <- randomBytes 8
  pure $ UID bytes
```

* Runner:

```Haskell

main :: IO ()
main = runEff $ do
  rng <- newCryptoRNGState
  result <- runCryptoRNG rng
            $ generateUID
  liftIO $ print result

```

See the [tests][tests] to see an example use.

[effectful]: https://github.com/arybczak/effectful
[tests]: https://github.com/Kleidukos/effectful-contrib/blob/main/effectful-crypto-rng/test/Main.hs
