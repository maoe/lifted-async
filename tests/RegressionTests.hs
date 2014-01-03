{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad (when)
import Data.Function (fix)
import Data.IORef
import Foreign.C.Types (CUInt(..))

import Control.Concurrent.Async.Lifted

import Test.Tasty.TH
import Test.Tasty.HUnit

main :: IO ()
main = $defaultMainGenerator

-- https://github.com/maoe/lifted-async/issues/1
case_issue1 :: Assertion
case_issue1 = do
  ref <- newIORef (5 :: Int)
  withAsync (zombie ref) $ \_ -> return ()
  n <- readIORef ref
  n @?= 5
  where
    zombie ref = fix $ \loop -> do
      n <- readIORef ref
      when (n > 0) $ do
        c_sleep 0
        writeIORef ref $! n - 1
        loop

foreign import ccall safe "sleep" c_sleep :: CUInt -> IO CUInt
