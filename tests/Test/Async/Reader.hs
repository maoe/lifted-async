{-# LANGUAGE TemplateHaskell #-}
module Test.Async.Reader
  ( readerTestGroup
  ) where
import Control.Monad.Reader
import Data.Maybe (isJust, isNothing)

import Control.Concurrent.Lifted
import Control.Exception.Lifted as E

import Control.Concurrent.Async.Lifted.Safe
import Test.Async.Common

readerTestGroup :: TestTree
readerTestGroup = $(testGroupGenerator)

case_async_waitCatch :: Assertion
case_async_waitCatch = do
  r <- flip runReaderT value $ do
    a <- async $ return value
    waitCatch a
  case r of
    Left _  -> assertFailure "An exception must not be raised."
    Right e -> do
      e @?= value

case_async_wait :: Assertion
case_async_wait = do
  r <- flip runReaderT value $ do
    a <- async $ return value
    wait a
  r @?= value

case_async_exwaitCatch :: Assertion
case_async_exwaitCatch = do
  r <- flip runReaderT value $ do
    a <- async $ throwIO TestException
    waitCatch a
  case r of
    Left e ->
      fromException e @?= Just TestException
    Right _ -> assertFailure "An exception must be raised."

case_async_exwait :: Assertion
case_async_exwait =
  void $ flip runReaderT value $ do
    a <- async $ throwIO TestException
    (wait a >> liftIO (assertFailure "An exception must be raised"))
      `E.catch` \e ->
        liftIO $ e @?= TestException

case_withAsync_waitCatch :: Assertion
case_withAsync_waitCatch =
  void $ flip runReaderT value $ do
    withAsync (return value) $ \a -> do
      r <- waitCatch a
      case r of
        Left _  -> liftIO $ assertFailure "An exception must not be raised."
        Right e -> do
          liftIO $ e @?= value

case_withAsync_wait2 :: Assertion
case_withAsync_wait2 = do
  r <- flip runReaderT value $ do
    a <- withAsync (threadDelay 1000000) $ return
    waitCatch a
  case r of
    Left e  -> do
      fromException e @?= Just ThreadKilled
    Right _ -> assertFailure "An exception must be raised."

case_async_cancel :: Assertion
case_async_cancel = sequence_ $ replicate 1000 run
  where
    run = do
      r <- flip runReaderT value $ do
        a <- async $ return value
        cancelWith a TestException
        waitCatch a
      case r of
        Left e ->
          fromException e @?= Just TestException
        Right r' ->
          r' @?= value

case_async_poll :: Assertion
case_async_poll =
  void $ flip runReaderT value $ do
    a <- async (threadDelay 1000000)
    r <- poll a
    when (isJust r) $
      liftIO $ assertFailure "The result must be nothing."
    r' <- poll a   -- poll twice, just to check we don't deadlock
    when (isJust r') $
      liftIO $ assertFailure "The result must be Nothing."

case_async_poll2 :: Assertion
case_async_poll2 =
  void $ flip runReaderT value $ do
    a <- async (return value)
    void $ wait a
    r <- poll a
    when (isNothing r) $
      liftIO $ assertFailure "The result must not be Nothing."
    r' <- poll a   -- poll twice, just to check we don't deadlock
    when (isNothing r') $
      liftIO $ assertFailure "The result must not be Nothing."

case_link :: Assertion
case_link = do
  r <- try $ flip runReaderT value $ do
    a <- async $ threadDelay 1000000 >> return value
    link a
    cancelWith a TestException
    wait a
  case r of
    Left e -> do
      fromException e @?= Just TestException
    Right _ -> assertFailure "An exception must be raised."
