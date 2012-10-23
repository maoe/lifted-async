module Main where
import Criterion.Main
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Async.Lifted as L

main :: IO ()
main = defaultMain
  [ bgroup "race"
      [ bench "async"        $ nfIO race_async
      , bench "lifted-async" $ nfIO race_liftedAsync
      ]
  , bgroup "concurrently"
      [ bench "async"        $ nfIO concurrently_async
      , bench "lifted-async" $ nfIO concurrently_liftedAsync
      ]
  , bgroup "mapConcurrently"
      [ bench "async"        $ nfIO mapConcurrently_async
      , bench "lifted-async" $ nfIO mapConcurrently_liftedAsync
      ]
  ]

race_async :: IO (Either Int Int)
race_async =
  A.race (return 1) (return 2)

race_liftedAsync :: IO (Either Int Int)
race_liftedAsync =
  L.race (return 1) (return 2)

concurrently_async :: IO (Int, Int)
concurrently_async =
  A.concurrently (return 1) (return 2)

concurrently_liftedAsync :: IO (Int, Int)
concurrently_liftedAsync =
  L.concurrently (return 1) (return 2)

mapConcurrently_async :: IO [Int]
mapConcurrently_async =
  A.mapConcurrently return [1..10]

mapConcurrently_liftedAsync :: IO [Int]
mapConcurrently_liftedAsync =
  L.mapConcurrently return [1..10]
