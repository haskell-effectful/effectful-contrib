# effectful-time 

## Description

A `Time` effect for the [`effectful`][effectful] ecosystem.

## How to use

This library exposes the following elements:

* `Time` — The type-level effect that you can declare in your type signatures.

```haskell
processTime :: (Time :> es) => Eff es UTCTime
```

* `getCurrentTime` — The function that you will call to get a `Eff es UTCTime`.

```haskell
import Data.Time (UTCTime)
import qualified Data.Time as T

import Effectful.Time (getCurrentTime)

usingTime :: (Time :> es) => Eff es UTCTime
usingTime = do
  t <- getCurrentTime
  pure $ T.addUTCTime 100 t
```

* Runners for IO & Pure environments:

```Haskell
runCurrentTimeIO usingTime
-- or
runCurrentTimePure (time :: UTCTime) usingTime
```

See the [tests][tests] to see an example use.

[effectful]: https://github.com/arybczak/effectful
[tests]: https://github.com/Kleidukos/effectful-contrib/blob/main/effectful-time/test/Main.hs
