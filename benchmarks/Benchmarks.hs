module Main where
import Control.Exception (SomeException(..))

import Criterion.Main
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Async.Lifted as L
import qualified Control.Concurrent.Async.Lifted.Safe as LS

main :: IO ()
main = defaultMain
  [ bgroup "async-wait"
      [ bench "async" $ whnfIO asyncWait_async
      , bench "lifted-async" $ whnfIO asyncWait_liftedAsync
      , bench "lifted-async-safe" $ whnfIO asyncWait_liftedAsyncSafe
      ]
  , bgroup "async-cancel-waitCatch"
      [ bench "async" $ whnfIO asyncCancelWaitCatch_async
      , bench "lifted-async" $ whnfIO asyncCancelWaitCatch_liftedAsync
      , bench "lifted-async-safe" $ whnfIO asyncCancelWaitCatch_liftedAsyncSafe
      ]
  , bgroup "waitAny"
      [ bench "async" $ whnfIO waitAny_async
      , bench "lifted-async" $ whnfIO waitAny_liftedAsync
      , bench "lifted-async-safe" $ whnfIO waitAny_liftedAsyncSafe
      ]
  , bgroup "race"
      [ bench "async" $ nfIO race_async
      , bench "lifted-async" $ nfIO race_liftedAsync
      , bench "lifted-async-safe" $ nfIO race_liftedAsyncSafe
      , bench "async (inlined)" $ nfIO race_async_inlined
      , bench "lifted-async (inlined)" $ nfIO race_liftedAsync_inlined
      ]
  , bgroup "concurrently"
      [ bench "async" $ nfIO concurrently_async
      , bench "lifted-async" $ nfIO concurrently_liftedAsync
      , bench "lifted-async-safe" $ nfIO concurrently_liftedAsyncSafe
      , bench "async (inlined)" $ nfIO concurrently_async_inlined
      , bench "lifted-async (inlined)" $ nfIO concurrently_liftedAsync_inlined
      ]
  , bgroup "mapConcurrently"
      [ bench "async" $ nfIO mapConcurrently_async
      , bench "lifted-async" $ nfIO mapConcurrently_liftedAsync
      , bench "lifted-async-safe" $ nfIO mapConcurrently_liftedAsyncSafe
      ]
  ]

asyncWait_async :: IO Int
asyncWait_async = do
  a <- A.async (return 1)
  A.wait a

asyncWait_liftedAsync :: IO Int
asyncWait_liftedAsync = do
  a <- L.async (return 1)
  L.wait a

asyncWait_liftedAsyncSafe :: IO Int
asyncWait_liftedAsyncSafe = do
  a <- LS.async (return 1)
  LS.wait a

asyncCancelWaitCatch_async :: IO (Either SomeException Int)
asyncCancelWaitCatch_async = do
  a <- A.async (return 1)
  A.cancel a
  A.waitCatch a

asyncCancelWaitCatch_liftedAsync :: IO (Either SomeException Int)
asyncCancelWaitCatch_liftedAsync = do
  a <- L.async (return 1)
  L.cancel a
  L.waitCatch a

asyncCancelWaitCatch_liftedAsyncSafe :: IO (Either SomeException Int)
asyncCancelWaitCatch_liftedAsyncSafe = do
  a <- LS.async (return 1)
  LS.cancel a
  LS.waitCatch a

waitAny_async :: IO Int
waitAny_async = do
  as <- mapM (A.async . return) [1..10]
  (_, n) <- A.waitAny as
  return n

waitAny_liftedAsync :: IO Int
waitAny_liftedAsync = do
  as <- mapM (L.async . return) [1..10]
  (_, n) <- L.waitAny as
  return n

waitAny_liftedAsyncSafe :: IO Int
waitAny_liftedAsyncSafe = do
  as <- mapM (LS.async . return) [1..10]
  (_, n) <- LS.waitAny as
  return n

race_async :: IO (Either Int Int)
race_async =
  A.race (return 1) (return 2)

race_liftedAsync :: IO (Either Int Int)
race_liftedAsync =
  L.race (return 1) (return 2)

race_liftedAsyncSafe :: IO (Either Int Int)
race_liftedAsyncSafe =
  LS.race (return 1) (return 2)

race_async_inlined :: IO (Either Int Int)
race_async_inlined =
  A.withAsync (return 1) $ \a ->
    A.withAsync (return 2) $ \b ->
      A.waitEither a b

race_liftedAsync_inlined :: IO (Either Int Int)
race_liftedAsync_inlined =
  L.withAsync (return 1) $ \a ->
    L.withAsync (return 2) $ \b ->
      L.waitEither a b

concurrently_async :: IO (Int, Int)
concurrently_async =
  A.concurrently (return 1) (return 2)

concurrently_liftedAsync :: IO (Int, Int)
concurrently_liftedAsync =
  L.concurrently (return 1) (return 2)

concurrently_liftedAsyncSafe :: IO (Int, Int)
concurrently_liftedAsyncSafe =
  LS.concurrently (return 1) (return 2)

concurrently_async_inlined :: IO (Int, Int)
concurrently_async_inlined =
  A.withAsync (return 1) $ \a ->
    A.withAsync (return 2) $ \b ->
      A.waitBoth a b

concurrently_liftedAsync_inlined :: IO (Int, Int)
concurrently_liftedAsync_inlined =
  L.withAsync (return 1) $ \a ->
    L.withAsync (return 2) $ \b ->
      L.waitBoth a b

mapConcurrently_async :: IO [Int]
mapConcurrently_async =
  A.mapConcurrently return [1..10]

mapConcurrently_liftedAsync :: IO [Int]
mapConcurrently_liftedAsync =
  L.mapConcurrently return [1..10]

mapConcurrently_liftedAsyncSafe :: IO [Int]
mapConcurrently_liftedAsyncSafe =
  LS.mapConcurrently return [1..10]
