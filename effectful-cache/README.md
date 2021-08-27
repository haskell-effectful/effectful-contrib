# Effectful-cache 

## Description

A `Cache` effect for the [`effectful`][effectful] ecosystem.

## How to use

This library exposes the following elements:

* `Cache` — The type-level effect that you can declare in your type signatures.

```
populateIntCache :: (Cache Int Int :> es) => Eff es ()
```

* `insert`, `lookup`, `keys`, `delete`, `filterWithKey` – Operations on `Cache`. They should always be used with Type Applications when using literals:

```Haskell
insertAndLookup :: (Cache Int Int :> es) => Eff es (Maybe Int)
insertAndLookup = do
  insert @Int @Int 3 12
  lookup @Int 3

listKeys :: (Cache Int Int :> es) => Eff es [Int]
listKeys = do
  populateIntCache
  keys @Int @Int
```

* An IO Runner

```
runCacheIO (cache :: Data.Cache Int Int)
```

See the [tests][tests] to see an example use.

[effectful]: https://github.com/arybczak/effectful
[tests]: https://github.com/Kleidukos/effectful-cache/blob/main/test/Main.hs
