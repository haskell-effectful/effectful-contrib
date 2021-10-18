# effectful-log-base

## Description

This package provides a `Logging` effect for the [`effectful`][effectful]
ecosystem based on the [`log-base`][log-base] library.
In addition to that it contains an instance of
[`Log.Class.MonadLog`](https://hackage.haskell.org/package/log-base-0.10.0.1/docs/Log-Class.html#t:MonadLog)
for the `Eff` monad.

## How to use

This library provides the following modules:

- `Effectful.Log`
  This is module contains the core of the `effectful-log-base`. In particular,
  the `Logging` effect and the functions associated with it are defined here.
  Additionally, this module provides a `MonadLog` instance for the `Eff` monad.
- `Effectful.Log.Backend.*`
  The modules in this namespace provide lifted versions of the functions found
  in the corresponding namespace of the `log-base` package.
- `Effectful.Log.Logger`
  This module contains functions which are useful if you want to implement
  custom loggers.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Effectful.Monad
import Effectful.Time (Time, runCurrentTimeIO)
import Log (LogLevel(LogInfo), logInfo)

import Effectful.Log
import Effectful.Log.Backend.StandardOutput

main :: IO ()
main = runEff $ do
  withSimpleStdOutLogger $ \logger -> do
    runCurrentTimeIO . runLogging "main" logger LogInfo $ do
      app

app :: (Logging :> es, Time :> es) => Eff es ()
app = do
  logInfo "Hello !" ("Some JSON payload" :: Text)
```

[effectful]: https://github.com/arybczak/effectful
[log-base]: https://hackage.haskell.org/package/log-base
