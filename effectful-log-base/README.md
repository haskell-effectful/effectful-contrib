# effectful-log-base

## Description

This package provides a `Logging` effect for the [`effectful`][effectful]
ecosystem based on the [`log-base`][log-base] library.
In addition to that it contains an instance of
[`Log.Class.MonadLog`](https://hackage.haskell.org/package/log-base-0.10.0.1/docs/Log-Class.html#t:MonadLog)
for the `Eff` monad.

## How to use


```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Effectful.Monad
import Log (logInfo)

import Effectful.Log
import Effectful.Log.Backend.StandardOutput

main :: IO ()
main = runEff $ do
  withSimpleStdOutLogger $ \logger -> do
    runLogging "main" logger $ do
      app

app :: Logging :> es => Eff es ()
app = do
  logInfo "Hello !" ("Some JSON payload" :: Text)
```

[effectful]: https://github.com/arybczak/effectful
[log-base]: https://hackage.haskell.org/package/log-base
