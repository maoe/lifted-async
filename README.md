lifted-async
==========
[![Hackage](https://img.shields.io/hackage/v/lifted-async.svg)](https://hackage.haskell.org/package/lifted-async)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/lifted-async.svg)](http://packdeps.haskellers.com/feed?needle=lifted-async)
[![lifted-async on Stackage LTS 3](http://stackage.org/package/lifted-async/badge/lts)](http://stackage.org/lts/package/lifted-async)
[![Build Status](https://secure.travis-ci.org/maoe/lifted-async.png)](http://travis-ci.org/maoe/lifted-async)
[![Gitter chat](https://badges.gitter.im/maoe/lifted-async.png)](https://gitter.im/maoe/lifted-async)

This package provides IO operations from [async](http://hackage.haskell.org/package/async) package lifted to any instance of `MonadBase` or `MonadBaseControl` from [monad-control](http://hackage.haskell.org/package/monad-control) package.

You can install this library using cabal:

```
cabal install lifted-async
```

Contact information
==========

This library is written and maintained by Mitsutoshi Aoe <maoe@foldr.in>.
[Pull requests](https://github.com/maoe/lifted-async/pulls) and [bug reports](https://github.com/maoe/lifted-async/issues) are welcome. A chat room is available on [Gitter](https://gitter.im/maoe/lifted-async).
