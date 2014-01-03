{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}
module Main where
import Test.Tasty (defaultMain, testGroup)

import Test.Async.IO
import Test.Async.State

main :: IO ()
main = defaultMain $ testGroup "lifted-async test suite"
  [ ioTestGroup
  , stateTestGroup
  ]
