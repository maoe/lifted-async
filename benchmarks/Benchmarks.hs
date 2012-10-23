module Main where
import Criterion.Main
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Async.Lifted as L

main :: IO ()
main = defaultMain
  [ bgroup "race"
      [ bench "async" race_async
      , bench "lifted-async" race_liftedAsync
      ]
  , bgroup "concAsync"
      [ bench "async" concAsync_async
      , bench "lifted-async" concAsync_liftedAsync
      ]
  ]

race_async :: IO ()
race_async = whnfIO $ A.race (return 1) (return 2)

race_liftedAsync :: IO ()
race_liftedAsync = whnfIO $ L.race (return 1) (return 2)

concAsync_async :: IO ()
concAsync_async = whnfIO $
  A.withAsync (return 1) $ \a ->
    A.withAsync (return 2) $ \b ->
      A.waitBoth a b

concAsync_liftedAsync :: IO ()
concAsync_liftedAsync = whnfIO $
  L.withAsync (return 1) $ \a ->
    L.withAsync (return 2) $ \b ->
      L.waitBoth a b
