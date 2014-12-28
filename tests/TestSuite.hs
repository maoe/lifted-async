{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}
module Main where
import Test.Tasty (defaultMain, testGroup)

import Test.Async.IO
import Test.Async.State
import Test.Async.Reader

main :: IO ()
main = defaultMain $ testGroup "lifted-async test suite"
  [ ioTestGroup
  , stateTestGroup
  , readerTestGroup
  ]
