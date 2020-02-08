# Revision history for lifted-async

## v0.10.0.5 - 2020-02-08

* Relax upper version bounds for constraints

## v0.10.0.4 - 2019-05-03

* Relax upper version bounds for base and constraints

## v0.10.0.3 - 2018-09-25

* Relax upper version bound for base to support GHC 8.6.1

## v0.10.0.2 - 2018-05-13

* Allow test_link to fail because it's non-deterministic (#26)

## v0.10.0.1 - 2018-03-10

* Relax upper version bound for base in GHC 8.4.1 (#25)

## v0.10.0 - 2018-02-08

* Support only async >= 2.2
* Drop support for monad-control == 0.*
* Drop support for GHC < 7.10

## v0.9.3.3 - 2018-01-22

* Relax upper version bound for constraints

## v0.9.3.2 - 2017-12-12

* Minor improvements in the cabal file

## v0.9.3.1 - 2017-12-12

* Relax upper version bound for tasty-hunit

## v0.9.3 - 2017-06-26

* Add Haddock comments for concurrently_ (#23)
* Add replicateConcurrently and replicateConcurrently_
* Test with GHC 8.2.1 on Travis

## v0.9.2 - 2017-06-24

* Add concurrently_ (#22)

## v0.9.1.1 - 2017-01-26

* Relax upper version bound for constraints

## v0.9.1 - 2017-01-13

* Add (for|map)Concurrently_ (#21)

## v0.9.0 - 2016-05-22

* Leverage `StM m a ~ a` in the `Safe` module for faster `wait`/`poll`/`race`/`concurrently`

## v0.8.0.1 - 2015-01-17

* Relax upper bound for constraints

## v0.8.0 - 2016-01-10

* Drop Monad instance for Concurrently
* Expose STM operations
* Relax upper bound for base and async
* Add Monoid and Semigroup instances for Concurrently

## v0.7.0.2 - 2015-11-26

* Relax upper bound for the constraints package
    * Upper bound remains < 0.6 for GHC < 7.8 as constraints-0.6 requires the closed type families extension.
* Drop support for GHC 7.4.2

## v0.7.0.1 - 2015-05-18

* Fix typecheck error with GHC HEAD (#17)

## v0.7.0 - 2015-03-30

* Fix the unnecessarily constrained type of link2 (#16)
* Turn the caveat in the Safe module into a WARNING pragma (#15)

## v0.6.0.1 - 2015-01-14

* Increase the lower bound for base to >= 4.5

## v0.6.0 - 2015-01-13

* Replace `StM m a ~ a` in the type signatures with `Forall (Pure m)` (#12)

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
