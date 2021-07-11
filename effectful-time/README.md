# effectful-time 

## Description

A `Time` effect for the [`effectful`][effectful] ecosystem.

## How to use

This library exposes these elements:

* `Time` — The type-level effect that you can declare in your type signatures.

```haskell
storingTimeInState :: (Time :> es, State UTCTime :> es) => Eff es UTCTime
```

* `getCurrentTime` — The function that you will call to get a `Eff es UTCTime`.

```haskell
action :: (Time :> es) => Eff es UTCTime
action = do
  t <- getCurrentTime -- from Data.Time.Effect
  pure $ T.addUTCTime 100 t -- from Data.Time.Clock, qualified with `T.`
```

* Runners for IO & Pure environments:

```Haskell

```

See the [tests][tests] to see an example use.

[effectful]: https://github.com/arybczak/effectful
[tests]: https://github.com/Kleidukos/effectful-time/blob/main/test/Main.hs
