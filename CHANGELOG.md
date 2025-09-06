# Revision history for lifted-async

## v0.11.0 - 2025-09-06

* Drop support for GHC 7 and allow base-4.22 ([#47](https://github.com/maoe/lifted-async/pull/47))

## v0.10.2.7 - 2024-11-03

* Allow base-4.21, tasty-bench-0.4, bump Haskell CI to GHC 9.12.0 ([#46](https://github.com/maoe/lifted-async/pull/46))

## v0.10.2.6 - 2024-10-05

* Allow base-4.20, bump CI to GHC 9.10.1 (([#44](https://github.com/maoe/lifted-async/issues/44)))

## v0.10.2.5 - 2023-11-11

* Support GHC 9.8 ([#42](https://github.com/maoe/lifted-async/issues/42))
* Allow base-4.19, bump CI to GHC 9.8.1 ([#43](https://github.com/maoe/lifted-async/pull/43))

## v0.10.2.4 - 2022-03-19

* Support mtl-2.3.1, allow base-4.18 (GHC 9.6) ([#41](https://github.com/maoe/lifted-async/pull/41))

## v0.10.2.3 - 2022-08-13

* Allow base-4.17 (GHC 9.4) and bump CI to latest GHC versions ([#39](https://github.com/maoe/lifted-async/pull/39))

## v0.10.2.2 - 2021-11-02

* Allow base-4.17 for GHC 9.2.1 ([#37](https://github.com/maoe/lifted-async/pull/37))

## v0.10.2.1 - 2021-07-23

* Relax upper version bound for tasty-bench

## v0.10.2 - 2021-04-02

* Define withAsync in terms of corresponding function from async ([#36](https://github.com/maoe/lifted-async/pull/36))
  * Fixes [#34](https://github.com/maoe/lifted-async/issues/34)

## v0.10.1.3 - 2021-02-26

* Support GHC 9.0.1 ([#33](https://github.com/maoe/lifted-async/pull/33))
* Switch from Travis CI to GitHub Actions
* Switch from criterion to tasty-bench

## v0.10.1.2 - 2020-07-23

* Relax upper version bound for tasty-expected-failure

## v0.10.1.1 - 2020-06-29

* Bump up cabal-version to 1.24

## v0.10.1 - 2020-06-29

* Fix typechecking errors with GHC HEAD 8.11 ([#31](https://github.com/maoe/lifted-async/pull/31))

## v0.10.0.6 - 2020-03-31

* Relax upper version bound for base to suppose GHC 8.10 ([#30](https://github.com/maoe/lifted-async/pull/30))

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
