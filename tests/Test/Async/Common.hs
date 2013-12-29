{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Async.Common
  ( value
  , TestException(..)
  , module X
  ) where

import Data.Typeable

import Control.Concurrent.Lifted (threadDelay)
import Control.Exception.Lifted
import Control.Monad.Trans.Control
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.TH as X
import Test.HUnit as X hiding (Test)

import Control.Concurrent.Async.Lifted as X

value :: Int
value = 42

data TestException = TestException
  deriving (Eq, Show, Typeable)

instance Exception TestException
