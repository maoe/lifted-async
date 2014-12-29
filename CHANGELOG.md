## v0.5.0.1 - 2014-12-29

* Fix build issues in the test suite (#11 and others)

## v0.5.0 - 2014-12-29

* Simplify the type of `Concurrently` (#10)

## v0.4.0 - 2014-12-29

* Accept `constraints > 0.4` as well even when built with ghc < 7.8.
* Support for GHC 7.10.1

## v0.3.0 - 2014-12-28

* Support for `monad-control == 1.0.*`
    * `waitEither_` and `race_` now discard monadic effects besides `IO`. This is a breaking change.
    * `Control.Concurrent.Async.Lifted.Safe` is added.
* Add `Monad` instance for `Concurrently`
* Relax upper bound for base

## v0.2.0.2 - 2014-08-20

* Fix build failure in the test suite (#6)

## v0.2.0.1 - 2014-07-26

* Fix a typo in a haddock comment (#5 by @supki)
* Fix Travis CI failure

## v0.2.0 - 2014-05-01

* Generalize `Concurrently` (#4)
