{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Control.Concurrent.Async.Lifted
Copyright   : Copyright (C) 2012-2018 Mitsutoshi Aoe
License     : BSD-style (see the file LICENSE)
Maintainer  : Mitsutoshi Aoe <maoe@foldr.in>
Stability   : experimental

This is a wrapped version of @Control.Concurrent.Async@ with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.

All the functions restore the monadic effects in the forked computation
unless specified otherwise.

#if MIN_VERSION_monad_control(1, 0, 0)
If your monad stack satisfies @'StM' m a ~ a@ (e.g. the reader monad), consider
using @Control.Concurrent.Async.Lifted.Safe@ module, which prevents you from
messing up monadic effects.
#endif
-}

module Control.Concurrent.Async.Lifted
  ( -- * Asynchronous actions
    A.Async
    -- ** Spawning
  , async, asyncBound, asyncOn
  , asyncWithUnmask, asyncOnWithUnmask

    -- ** Spawning with automatic 'cancel'ation
  , withAsync, withAsyncBound, withAsyncOn
  , withAsyncWithUnmask, withAsyncOnWithUnmask

    -- ** Quering 'Async's
  , wait, poll, waitCatch
  , cancel
  , uninterruptibleCancel
  , cancelWith
  , A.asyncThreadId
  , A.AsyncCancelled(..)

    -- ** STM operations
  , A.waitSTM, A.pollSTM, A.waitCatchSTM

    -- ** Waiting for multiple 'Async's
  , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
  , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel
  , waitEither_
  , waitBoth

    -- ** Waiting for multiple 'Async's in STM
  , A.waitAnySTM
  , A.waitAnyCatchSTM
  , A.waitEitherSTM
  , A.waitEitherCatchSTM
  , A.waitEitherSTM_
  , A.waitBothSTM

    -- ** Linking
  , link, link2
  , A.ExceptionInLinkedThread(..)

    -- * Convenient utilities
  , race, race_, concurrently, concurrently_
  , mapConcurrently, mapConcurrently_
  , forConcurrently, forConcurrently_
  , replicateConcurrently, replicateConcurrently_
  , Concurrently(..)

  , A.compareAsyncs
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad ((>=>), forever, fmap, void)
import Data.Foldable (fold)
import GHC.IO (unsafeUnmask)
import Prelude

import Control.Concurrent.Async (Async)
import Control.Exception.Lifted (SomeException, Exception)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control
import qualified Control.Concurrent.Async as A
import qualified Control.Exception.Lifted as E

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Traversable
#endif
#if !MIN_VERSION_base(4, 8, 0)
import Data.Monoid (Monoid(mappend, mempty))
#elif MIN_VERSION_base(4, 9, 0)
import Data.Semigroup (Semigroup((<>)))
#endif

-- | Generalized version of 'A.async'.
async :: MonadBaseControl IO m => m a -> m (Async (StM m a))
async = asyncUsing A.async

-- | Generalized version of 'A.asyncBound'.
asyncBound :: MonadBaseControl IO m => m a -> m (Async (StM m a))
asyncBound = asyncUsing A.asyncBound

-- | Generalized version of 'A.asyncOn'.
asyncOn :: MonadBaseControl IO m => Int -> m a -> m (Async (StM m a))
asyncOn cpu = asyncUsing (A.asyncOn cpu)

-- | Generalized version of 'A.asyncWithUnmask'.
asyncWithUnmask
  :: MonadBaseControl IO m
  => ((forall b. m b -> m b) -> m a)
  -> m (Async (StM m a))
asyncWithUnmask actionWith =
  asyncUsing A.async (actionWith (liftBaseOp_ unsafeUnmask))

-- | Generalized version of 'A.asyncOnWithUnmask'.
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

-- | Generalized version of 'A.withAsync'.
withAsync
  :: MonadBaseControl IO m
  => m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsync = withAsyncUsing async
{-# INLINABLE withAsync #-}

-- | Generalized version of 'A.withAsyncBound'.
withAsyncBound
  :: MonadBaseControl IO m
  => m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncBound = withAsyncUsing asyncBound
{-# INLINABLE withAsyncBound #-}

-- | Generalized version of 'A.withAsyncOn'.
withAsyncOn
  :: MonadBaseControl IO m
  => Int
  -> m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncOn = withAsyncUsing . asyncOn
{-# INLINABLE withAsyncOn #-}

-- | Generalized version of 'A.withAsyncWithUnmask'.
withAsyncWithUnmask
  :: MonadBaseControl IO m
  => ((forall c. m c -> m c) -> m a)
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncWithUnmask actionWith =
  withAsyncUsing async (actionWith (liftBaseOp_ unsafeUnmask))
{-# INLINABLE withAsyncWithUnmask #-}

-- | Generalized version of 'A.withAsyncOnWithUnmask'.
withAsyncOnWithUnmask
  :: MonadBaseControl IO m
  => Int
  -> ((forall c. m c -> m c) -> m a)
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncOnWithUnmask cpu actionWith =
  withAsyncUsing (asyncOn cpu) (actionWith (liftBaseOp_ unsafeUnmask))
{-# INLINABLE withAsyncOnWithUnmask #-}

withAsyncUsing
  :: MonadBaseControl IO m
  => (m a -> m (Async (StM m a)))
  -> m a
  -> (Async (StM m a) -> m b)
  -> m b
withAsyncUsing fork action inner = E.mask $ \restore -> do
  a <- fork $ restore action
  r <- restore (inner a) `E.catch` \e -> do
    cancel a
    E.throwIO (e :: SomeException)
  cancel a
  return r

-- | Generalized version of 'A.wait'.
wait :: MonadBaseControl IO m => Async (StM m a) -> m a
wait = liftBase . A.wait >=> restoreM

-- | Generalized version of 'A.poll'.
poll
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> m (Maybe (Either SomeException a))
poll a =
  liftBase (A.poll a) >>=
  maybe (return Nothing) (fmap Just . sequenceEither)

-- | Generalized version of 'A.cancel'.
cancel :: MonadBase IO m => Async a -> m ()
cancel = liftBase . A.cancel

-- | Generalized version of 'A.cancelWith'.
cancelWith :: (MonadBase IO m, Exception e) => Async a -> e -> m ()
cancelWith = (liftBase .) . A.cancelWith

-- | Generalized version of 'A.uninterruptibleCancel'.
uninterruptibleCancel :: MonadBase IO m => Async a -> m ()
uninterruptibleCancel = liftBase . A.uninterruptibleCancel

-- | Generalized version of 'A.waitCatch'.
waitCatch
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> m (Either SomeException a)
waitCatch a = liftBase (A.waitCatch a) >>= sequenceEither

-- | Generalized version of 'A.waitAny'.
waitAny :: MonadBaseControl IO m => [Async (StM m a)] -> m (Async (StM m a), a)
waitAny as = do
  (a, s) <- liftBase $ A.waitAny as
  r <- restoreM s
  return (a, r)

-- | Generalized version of 'A.waitAnyCatch'.
waitAnyCatch
  :: MonadBaseControl IO m
  => [Async (StM m a)]
  -> m (Async (StM m a), Either SomeException a)
waitAnyCatch as = do
  (a, s) <- liftBase $ A.waitAnyCatch as
  r <- sequenceEither s
  return (a, r)

-- | Generalized version of 'A.waitAnyCancel'.
waitAnyCancel
  :: MonadBaseControl IO m
  => [Async (StM m a)]
  -> m (Async (StM m a), a)
waitAnyCancel as = do
  (a, s) <- liftBase $ A.waitAnyCancel as
  r <- restoreM s
  return (a, r)

-- | Generalized version of 'A.waitAnyCatchCancel'.
waitAnyCatchCancel
  :: MonadBaseControl IO m
  => [Async (StM m a)]
  -> m (Async (StM m a), Either SomeException a)
waitAnyCatchCancel as = do
  (a, s) <- liftBase $ A.waitAnyCatchCancel as
  r <- sequenceEither s
  return (a, r)

-- | Generalized version of 'A.waitEither'.
waitEither
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either a b)
waitEither a b =
  liftBase (A.waitEither a b) >>=
  either (fmap Left . restoreM) (fmap Right . restoreM)

-- | Generalized version of 'A.waitEitherCatch'.
waitEitherCatch
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (fmap Left . sequenceEither) (fmap Right . sequenceEither)

-- | Generalized version of 'A.waitEitherCancel'.
waitEitherCancel
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either a b)
waitEitherCancel a b =
  liftBase (A.waitEitherCancel a b) >>=
  either (fmap Left . restoreM) (fmap Right . restoreM)

-- | Generalized version of 'A.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (fmap Left . sequenceEither) (fmap Right . sequenceEither)

-- | Generalized version of 'A.waitEither_'.
--
-- NOTE: This function discards the monadic effects besides IO in the forked
-- computation.
waitEither_
  :: MonadBase IO m
  => Async a
  -> Async b
  -> m ()
waitEither_ a b = liftBase (A.waitEither_ a b)

-- | Generalized version of 'A.waitBoth'.
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
{-# INLINABLE waitBoth #-}

-- | Generalized version of 'A.link'.
link :: MonadBase IO m => Async a -> m ()
link = liftBase . A.link

-- | Generalized version of 'A.link2'.
link2 :: MonadBase IO m => Async a -> Async b -> m ()
link2 = (liftBase .) . A.link2

-- | Generalized version of 'A.race'.
race :: MonadBaseControl IO m => m a -> m b -> m (Either a b)
race left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither a b
{-# INLINABLE race #-}

-- | Generalized version of 'A.race_'.
--
-- NOTE: This function discards the monadic effects besides IO in the forked
-- computation.
race_ :: MonadBaseControl IO m => m a -> m b -> m ()
race_ left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither_ a b
{-# INLINABLE race_ #-}

-- | Generalized version of 'A.concurrently'.
concurrently :: MonadBaseControl IO m => m a -> m b -> m (a, b)
concurrently left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitBoth a b
{-# INLINABLE concurrently #-}

-- | Generalized version of 'A.concurrently_'.
concurrently_ :: MonadBaseControl IO m => m a -> m b -> m ()
concurrently_ left right = void $ concurrently left right
{-# INLINABLE concurrently_ #-}

-- | Generalized version of 'A.mapConcurrently'.
mapConcurrently
  :: (Traversable t, MonadBaseControl IO m)
  => (a -> m b)
  -> t a
  -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- | Generalized version of 'A.mapConcurrently_'.
mapConcurrently_
  :: (Foldable t, MonadBaseControl IO m)
  => (a -> m b)
  -> t a
  -> m ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)

-- | Generalized version of 'A.forConcurrently'.
forConcurrently
  :: (Traversable t, MonadBaseControl IO m)
  => t a
  -> (a -> m b)
  -> m (t b)
forConcurrently = flip mapConcurrently

-- | Generalized version of 'A.forConcurrently_'.
forConcurrently_
  :: (Foldable t, MonadBaseControl IO m)
  => t a
  -> (a -> m b)
  -> m ()
forConcurrently_ = flip mapConcurrently_

-- | Generalized version of 'A.replicateConcurrently'.
replicateConcurrently
  :: MonadBaseControl IO m
  => Int
  -> m a
  -> m [a]
replicateConcurrently n =
  runConcurrently . sequenceA . replicate n . Concurrently

-- | Generalized version of 'A.replicateConcurrently_'.
replicateConcurrently_
  :: MonadBaseControl IO m
  => Int
  -> m a
  -> m ()
replicateConcurrently_ n =
  runConcurrently . fold . replicate n . Concurrently . void

-- | Generalized version of 'A.Concurrently'.
--
-- A value of type @'Concurrently' m a@ is an IO-based operation that can be
-- composed with other 'Concurrently' values, using the 'Applicative' and
-- 'Alternative' instances.
--
-- Calling 'runConcurrently' on a value of type @'Concurrently' m a@ will
-- execute the IO-based lifted operations it contains concurrently, before
-- delivering the result of type 'a'.
--
-- For example
--
-- @
--   (page1, page2, page3) <- 'runConcurrently' $ (,,)
--     '<$>' 'Concurrently' (getURL "url1")
--     '<*>' 'Concurrently' (getURL "url2")
--     '<*>' 'Concurrently' (getURL "url3")
-- @
newtype Concurrently m a = Concurrently { runConcurrently :: m a }

instance Functor m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance MonadBaseControl IO m => Applicative (Concurrently m) where
  pure = Concurrently . pure
  Concurrently fs <*> Concurrently as =
    Concurrently $ uncurry ($) <$> concurrently fs as

instance MonadBaseControl IO m => Alternative (Concurrently m) where
  empty = Concurrently $ liftBaseWith $ const (forever $ threadDelay maxBound)
  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

#if MIN_VERSION_base(4, 9, 0)
instance (MonadBaseControl IO m, Semigroup a) =>
  Semigroup (Concurrently m a) where
    (<>) = liftA2 (<>)

instance (MonadBaseControl IO m, Semigroup a, Monoid a) =>
  Monoid (Concurrently m a) where
    mempty = pure mempty
    mappend = (<>)
#else
instance (MonadBaseControl IO m, Monoid a) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = liftA2 mappend
#endif

sequenceEither :: MonadBaseControl IO m => Either e (StM m a) -> m (Either e a)
sequenceEither = either (return . Left) (fmap Right . restoreM)
