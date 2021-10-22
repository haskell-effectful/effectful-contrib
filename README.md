# effectful-contrib [![CI-badge][CI-badge]][CI-url]

This repository hosts bindings for the [`effectful`][effectful] library by
Andrzej Rybczak.

* [effectful-cache](./effectful-cache): A `Cache` effect to use the
  [cache library][cache].
* [effectful-log-base](./effectful-log-base): A `Logging` effect based on the
  [log-base library][log-base].
* [effectful-log-elasticsearch](./effectful-log-elasticsearch): An ElasticSearch
  backend for [effectful-log-base](./effectful-log-base) based on the
  [log-elasticsearch library][log-elasticsearch].
* [effectful-time](./effectful-time): A `Time` effect to use the
  [time library][time].
* [effectful-typed-process](./effectful-typed-process): A `Process` effect based
  on the [typed-process library][typed-process].

## Building and Testing

To build the libraries:

```
$ cabal build all
```

To run all the tests:

```
$ cabal test all
```

[effectful]: https://github.com/arybczak/effectful
[CI-badge]: https://img.shields.io/github/workflow/status/Kleidukos/effectful-contrib/CI?style=flat-square
[CI-url]: https://github.com/Kleidukos/effectful-contrib/actions
[cache]: https://hackage.haskell.org/package/cache
[log-base]: https://hackage.haskell.org/package/log-base
[log-elasticsearch]: https://hackage.haskell.org/package/log-elasticsearch
[time]: https://hackage.haskell.org/package/time
[typed-process]: https://hackage.haskell.org/package/typed-process
