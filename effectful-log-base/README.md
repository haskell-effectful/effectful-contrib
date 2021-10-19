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
  This is module contains the core of `effectful-log-base`. In particular, the
  `Logging` effect and the functions associated with it are defined here.
  Additionally, this module provides the `MonadLog` instance for the `Eff` monad.
- `Effectful.Log.Backend.*`
  The modules in this namespace provide lifted versions of the functions found
  in the corresponding namespace of the `log-base` package.
- `Effectful.Log.Logger`
  This module contains functions which are useful if you want to implement
  custom loggers.

To start using `effectful-log-base` package you must obtain a `Logger` which
serves as a sink for the log messages of your application.
To do so, you must choose a logging backend. This backend usually comes with a
function like `withSomeLogger :: (Logger -> Eff es a) -> Eff es a`.
Use this `Logger` along with the other configuration options to handle the
`Logging` effect with `Effectful.Log.runLogging`.

Log messages are written using one of the functions of the `log*` family found
in `Effectful.Log`. Since log messages have a timestamp emitting those will
incure a `Time :> es` constraint as well. Use one of the functions provided by
the [`effectful-time`][effectful-time] package to handle that effect.

Here is a full working example (also found in the `effectful-log-base/examples/`
directory of this repository):
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
[effectful-time]: https://github.com/Kleidukos/effectful-contrib/effectful-time
[log-base]: https://hackage.haskell.org/package/log-base
