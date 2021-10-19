# effectful-elasicsearch

## Description

This package provides an [ElasticSearch][elasticsearch] logging backend for the
[`effectful-log-base`][effectful-log-base] library.
It contains versions of the functions found in the
[`log-elasticsearch`][log-elasticsearch] package lifted to the `Eff` monad of
the [effectful][effectful] library.

## How to use

See the documentation of the
[`effectful-log-base`](https://github.com/Kleidukos/effectful-contrib/tree/main/effectful-log-base#readme)
package on how to use logging backends.

[effectful]: https://github.com/arybczak/effectful
[effectful-log-base]: https://github.com/Kleidukos/effectful-contrib/tree/main/effectful-log-base
[elasticsearch]: https://www.elastic.co/elasticsearch/
[log-elasticsearch]: https://hackage.haskell.org/package/log-elasticsearch
