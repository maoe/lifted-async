{-# LANGUAGE TemplateHaskell #-}
module Test.Async.State
  ( stateTestGroup
  , stateTestGroupExtra
  ) where
import Control.Monad (when)
import Control.Monad.State
import Data.Maybe (isJust, isNothing)

import Control.Concurrent.Lifted
import Control.Exception.Lifted

import Test.Async.Common

stateTestGroup :: Test
stateTestGroup = $(testGroupGenerator)

stateTestGroupExtra :: Test
stateTestGroupExtra =
  testGroup "async cancel rep" $
    replicate 1000 $ testCase "async cancel" async_cancel

case_async_waitCatch :: Assertion
case_async_waitCatch = do
  (r, s) <- flip runStateT value $ do
    a <- async $ modify (+1) >> return value
    waitCatch a
  case r of
    Left _  -> assertFailure ""
    Right e -> do
      e @?= value
      s @?= value + 1

case_async_wait :: Assertion
case_async_wait = do
  (r, s) <- flip runStateT value $ do
    a <- async $ modify (+1) >> return value
    wait a
  r @?= value
  s @?= value + 1

case_async_exwaitCatch :: Assertion
case_async_exwaitCatch = do
  (r, s) <- flip runStateT value $ do
    a <- async $ modify (+1) >> throwIO TestException
    waitCatch a
  case r of
    Left e  -> do
      fromException e @?= Just TestException
      s @?= value
    Right _ -> assertFailure ""

case_async_exwait :: Assertion
case_async_exwait =
  void $ flip runStateT value $ do
    a <- async $ modify (+1) >> throwIO TestException
    (wait a >> liftIO (assertFailure "")) `catch` \e -> do
      liftIO $ e @?= TestException
      s <- get
      liftIO $ s @?= value

case_withAsync_waitCatch :: Assertion
case_withAsync_waitCatch =
  void $ flip runStateT value $ do
    withAsync (modify (+1) >> return value) $ \a -> do
      r <- waitCatch a
      case r of
        Left _  -> liftIO $ assertFailure ""
        Right e -> do
          liftIO $ e @?= value
          s <- get
          liftIO $ s @?= value + 1

case_withAsync_wait2 :: Assertion
case_withAsync_wait2 = do
  (r, s) <- flip runStateT value $ do
    a <- withAsync (modify (+1) >> threadDelay 1000000) $ return
    waitCatch a
  case r of
    Left e  -> do
      fromException e @?= Just ThreadKilled
      s @?= value
    Right _ -> assertFailure ""

async_cancel :: Assertion
async_cancel = do
  (r, s) <- flip runStateT value $ do
    a <- async $ modify (+1) >> return value
    cancelWith a TestException
    waitCatch a
  case r of
    Left e -> do
      fromException e @?= Just TestException
      s @?= value
    Right r -> do
      r @?= value
      s @?= value + 1

case_async_poll :: Assertion
case_async_poll =
  void $ flip runStateT value $ do
    a <- async (threadDelay 1000000)
    r <- poll a
    when (isJust r) $ liftIO $ assertFailure ""
    r <- poll a   -- poll twice, just to check we don't deadlock
    when (isJust r) $ liftIO $ assertFailure ""

case_async_poll2 :: Assertion
case_async_poll2 =
  void $ flip runStateT value $ do
    a <- async (return value)
    wait a
    r <- poll a
    when (isNothing r) $ liftIO $ assertFailure ""
    r <- poll a   -- poll twice, just to check we don't deadlock
    when (isNothing r) $ liftIO $ assertFailure ""
