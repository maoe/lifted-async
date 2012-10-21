{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Control.Concurrent.Async.Lifted
  ( -- * Asynchronous actions
    A.Async
    -- ** Spawning
  , async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask

    -- ** Spawning with automatic 'cancel'ation
  , withAsync, withAsyncBound, withAsyncOn
  , withAsyncWithUnmask, withAsyncOnWithUnmask

    -- ** Quering 'Async's
  , wait, poll, waitCatch, cancel, cancelWith
  , A.asyncThreadId

    -- ** STM operations
  , A.waitSTM, A.pollSTM, A.waitCatchSTM

    -- ** Waiting for multiple 'Async's
  , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
  , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel
  , waitEither_
  , waitBoth

    -- ** Linking
  , link, link2

    -- ** Convenient utilities
  , race, race_, concurrently, mapConcurrently
  , A.Concurrently, A.runConcurrently
  ) where

import Control.Monad ((>=>), liftM)
import Data.Traversable (Traversable(..))
import GHC.IO (unsafeUnmask)
import Prelude hiding (mapM)

import Control.Concurrent.Async (Async)
import Control.Exception.Lifted (SomeException, Exception, bracket)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control
import qualified Control.Concurrent.Async as A

async :: MonadBaseControl IO m => m a -> m (Async (StM m a))
async = asyncUsing A.async

asyncBound :: MonadBaseControl IO m => m a -> m (Async (StM m a))
asyncBound = asyncUsing A.asyncBound

asyncOn :: MonadBaseControl IO m => Int -> m a -> m (Async (StM m a))
asyncOn cpu = asyncUsing (A.asyncOn cpu)

asyncWithUnmask
  :: MonadBaseControl IO m
  => ((forall b. m b -> m b) -> m a)
  -> m (Async (StM m a))
asyncWithUnmask actionWith =
  asyncUsing A.async (actionWith (liftBaseOp_ unsafeUnmask))

asyncOnWithUnmask
  :: MonadBaseControl IO m
  => Int
  -> ((forall b. m b -> m b) -> m a)
  -> m (Async (StM m a))
asyncOnWithUnmask cpu actionWith =
  asyncUsing (A.asyncOn cpu) (actionWith (liftBaseOp_ unsafeUnmask))

asyncUsing
  :: MonadBaseControl IO m
  => (IO (StM m a) -> IO (Async (StM m a)))
  -> m a
  -> m (Async (StM m a))
asyncUsing fork m =
  liftBaseWith $ \runInIO -> fork (runInIO m)

withAsync
  :: MonadBaseControl IO m
  => m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsync = withAsyncUsing async

withAsyncBound
  :: MonadBaseControl IO m
  => m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncBound = withAsyncUsing asyncBound

withAsyncOn
  :: MonadBaseControl IO m
  => Int
  -> m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncOn = withAsyncUsing . asyncOn

withAsyncWithUnmask
  :: MonadBaseControl IO m
  => ((forall c. m c -> m c) -> m a)
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncWithUnmask actionWith =
  withAsyncUsing async (actionWith (liftBaseOp_ unsafeUnmask))

withAsyncOnWithUnmask
  :: MonadBaseControl IO m
  => Int
  -> ((forall c. m c -> m c) -> m a)
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncOnWithUnmask cpu actionWith =
  withAsyncUsing (asyncOn cpu) (actionWith (liftBaseOp_ unsafeUnmask))

withAsyncUsing
  :: MonadBaseControl IO m
  => (m a -> m (Async (StM m a)))
  -> m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncUsing fork action = bracket (fork action) cancel

wait :: MonadBaseControl IO m => Async (StM m a) -> m a
wait = liftBase . A.wait >=> restoreM

poll
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> m (Maybe (Either SomeException a))
poll a =
  liftBase (A.poll a) >>=
  maybe (return Nothing) (liftM Just . sequenceEither)

waitCatch
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> m (Either SomeException a)
waitCatch a = liftBase (A.waitCatch a) >>= sequenceEither

cancel :: MonadBase IO m => Async (StM m a) -> m ()
cancel = liftBase . A.cancel

cancelWith :: (MonadBase IO m, Exception e) => Async (StM m a) -> e -> m ()
cancelWith = (liftBase .) . A.cancelWith

waitAny :: MonadBaseControl IO m => [Async (StM m a)] -> m (Async (StM m a), a)
waitAny as = do
  (a, s) <- liftBase $ A.waitAny as
  r <- restoreM s
  return (a, r)

waitAnyCatch
  :: MonadBaseControl IO m
  => [Async (StM m a)]
  -> m (Async (StM m a), Either SomeException a)
waitAnyCatch as = do
  (a, s) <- liftBase $ A.waitAnyCatch as
  r <- sequenceEither s
  return (a, r)

waitAnyCancel :: MonadBase IO m => [Async a] -> m (Async a, a)
waitAnyCancel = liftBase . A.waitAnyCancel

waitAnyCatchCancel
  :: MonadBaseControl IO m
  => [Async (StM m a)]
  -> m (Async (StM m a), Either SomeException a)
waitAnyCatchCancel as = do
  (a, s) <- liftBase $ A.waitAnyCatchCancel as
  r <- sequenceEither s
  return (a, r)

waitEither
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either a b)
waitEither a b =
  liftBase (A.waitEither a b) >>=
  either (liftM Left . restoreM) (liftM Right . restoreM)

waitEitherCatch
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (liftM Left . sequenceEither) (liftM Right . sequenceEither)

waitEitherCancel
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either a b)
waitEitherCancel a b =
  liftBase (A.waitEitherCancel a b) >>=
  either (liftM Left . restoreM) (liftM Right . restoreM)

waitEitherCatchCancel
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (liftM Left . sequenceEither) (liftM Right . sequenceEither)

waitEither_
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m ()
waitEither_ = (liftBase .) . A.waitEither_

waitBoth
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (a, b)
waitBoth a b = do
  (sa, sb) <- liftBase (A.waitBoth a b)
  ra <- restoreM sa
  rb <- restoreM sb
  return (ra, rb)

link :: MonadBase IO m => Async (StM m a) -> m ()
link = liftBase . A.link

link2 :: MonadBase IO m => Async (StM m a) -> Async (StM m b) -> m ()
link2 = (liftBase .) . A.link2

race :: MonadBaseControl IO m => m a -> m b -> m (Either a b)
race left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither a b

race_ :: MonadBaseControl IO m => m a -> m b -> m ()
race_ left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither_ a b

concurrently :: MonadBaseControl IO m => m a -> m b -> m (a, b)
concurrently left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitBoth a b

mapConcurrently
  :: (Traversable t, MonadBaseControl IO m)
  => (a -> m b)
  -> t a
  -> m (t b)
mapConcurrently f t =
  liftBaseWith (\runInIO ->
    A.runConcurrently $ traverse (A.Concurrently . runInIO . f) t) >>=
  mapM restoreM

sequenceEither :: MonadBaseControl IO m => Either e (StM m a) -> m (Either e a)
sequenceEither = either (return . Left) (liftM Right . restoreM)
