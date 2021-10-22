# effectful-log-postgres

## Description

This package provides a [PostgreSQL][postgres] logging backend for the
[`effectful-log-base`][effectful-log-base] library.
It contains a handler for the `Logging` effect and versions of the functions
found in the [`log-postgres`][log-postgres] package lifted to the `Eff` monad
of the [effectful][effectful] library.

## How to use

See the documentation of the
[`effectful-log-base`](./effectful-log-base#readme) package on how to use
logging backends.

[effectful]: https://github.com/arybczak/effectful
[effectful-log-base]: ./effectful-log-base
[log-postgres]: https://hackage.haskell.org/package/log-postgres
[postgres]: https://www.postgresql.org
