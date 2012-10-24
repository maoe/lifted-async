{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}
module Main where
import Test.Framework (defaultMain)

import Test.Async.IO
import Test.Async.State

main = defaultMain
  [ ioTestGroup
  , stateTestGroup
  ]
