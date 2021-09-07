# effectful-typed-process 

## Description

An alternative `Process` effect for the [`effectful`][effectful] ecosystem.
While to the `Process` effect shipped with the `effectful` library is based on
the [`process`][process] package this implementation relies on
[`typed-process`][typed-process] instead.

## How to use

The functions exposed by the `Effectful.Process.Typed` module are those from
[`System.Process.Typed`](https://hackage.haskell.org/package/typed-process-0.2.6.1/docs/System-Process-Typed.html)
with the notable difference that they have a `TypedProcess :> es` constraint.
Use `runTypedProcess` to handle the effect and eliminate the constraint.

```haskell
import Effectful.Monad
import Effectful.Process.Typed

main :: IO ()
main = runEff . runTypedProcess $ true

true :: TypedProcess :> es => Eff es ()
true = Effectful.Process.Typed.runProcess_ $ shell "true"
```

[effectful]: https://github.com/arybczak/effectful
[process]: https://hackage.haskell.org/package/process
[typed-process]: https://hackage.haskell.org/package/typed-process
