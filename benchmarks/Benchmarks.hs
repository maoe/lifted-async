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
race_async = nfIO $ A.race (return (1 :: Int)) (return (2 :: Int))

race_liftedAsync :: IO ()
race_liftedAsync = nfIO $ L.race (return (1 :: Int)) (return (2 :: Int))

concAsync_async :: IO ()
concAsync_async = nfIO $
  A.withAsync (return (1 :: Int)) $ \a ->
    A.withAsync (return (2 :: Int)) $ \b ->
      A.waitBoth a b

concAsync_liftedAsync :: IO ()
concAsync_liftedAsync = nfIO $
  L.withAsync (return (1 :: Int)) $ \a ->
    L.withAsync (return (2 :: Int)) $ \b ->
      L.waitBoth a b
