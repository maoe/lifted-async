{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Control.Concurrent.Async.Lifted
Copyright   : Copyright (C) 2012-2014 Mitsutoshi Aoe
License     : BSD-style (see the file LICENSE)
Maintainer  : Mitsutoshi Aoe <maoe@foldr.in>
Stability   : experimental

This is a wrapped version of "Control.Concurrent.Async" with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.

All the functions restore the monadic effects in the forked computation
unless specified otherwise.

#if MIN_VERSION_monad_control(1, 0, 0)
If your monad stack satisfies @'StM' m a ~ a@ (e.g. the reader monad), consider
using @Control.Concurrent.Async.Lifted.Safe@ module instead.
#endif
-}

module Control.Concurrent.Async.Lifted
  ( -- * Asynchronous actions
    A.Async
    -- ** Spawning
  , Safe.async, Safe.asyncBound, Safe.asyncOn
  , Safe.asyncWithUnmask, Safe.asyncOnWithUnmask

    -- ** Spawning with automatic 'cancel'ation
  , Safe.withAsync, Safe.withAsyncBound, Safe.withAsyncOn
  , Safe.withAsyncWithUnmask, Safe.withAsyncOnWithUnmask

    -- ** Quering 'Async's
  , wait, poll, waitCatch, cancel, cancelWith
  , Safe.asyncThreadId

    -- ** STM operations
  , Safe.waitSTM, Safe.pollSTM, Safe.waitCatchSTM

    -- ** Waiting for multiple 'Async's
  , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
  , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel
  , waitEither_
  , waitBoth

    -- ** Linking
  , Safe.link, Safe.link2

    -- * Convenient utilities
  , race, race_, concurrently, mapConcurrently
  , Concurrently(..)
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad ((>=>), forever, liftM)
import Data.Traversable (Traversable(..))
import Prelude hiding (mapM)

import Control.Concurrent.Async (Async)
import Control.Exception.Lifted (SomeException, Exception)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control
import qualified Control.Concurrent.Async as A

import qualified Control.Concurrent.Async.Lifted.Safe as Safe

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
  maybe (return Nothing) (liftM Just . sequenceEither)

-- | Generalized version of 'A.cancel'.
--
-- NOTE: This function discards the monadic effects besides IO in the forked
-- computation.
cancel :: MonadBase IO m => Async a -> m ()
cancel = Safe.cancel

-- | Generalized version of 'A.cancelWith'.
--
-- NOTE: This function discards the monadic effects besides IO in the forked
-- computation.
cancelWith :: (MonadBase IO m, Exception e) => Async a -> e -> m ()
cancelWith = Safe.cancelWith

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
  either (liftM Left . restoreM) (liftM Right . restoreM)

-- | Generalized version of 'A.waitEitherCatch'.
waitEitherCatch
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (liftM Left . sequenceEither) (liftM Right . sequenceEither)

-- | Generalized version of 'A.waitEitherCancel'.
waitEitherCancel
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either a b)
waitEitherCancel a b =
  liftBase (A.waitEitherCancel a b) >>=
  either (liftM Left . restoreM) (liftM Right . restoreM)

-- | Generalized version of 'A.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: MonadBaseControl IO m
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (liftM Left . sequenceEither) (liftM Right . sequenceEither)

-- | Generalized version of 'A.waitEither_'.
--
-- NOTE: This function discards the monadic effects besides IO in the forked
-- computation.
waitEither_
  :: MonadBaseControl IO m
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

-- | Generalized version of 'A.race'.
race :: MonadBaseControl IO m => m a -> m b -> m (Either a b)
race left right =
  Safe.withAsync left $ \a ->
  Safe.withAsync right $ \b ->
  waitEither a b
{-# INLINABLE race #-}

-- | Generalized version of 'A.race_'.
--
-- NOTE: This function discards the monadic effects besides IO in the forked
-- computation.
race_ :: MonadBaseControl IO m => m a -> m b -> m ()
race_ left right =
  Safe.withAsync left $ \a ->
  Safe.withAsync right $ \b ->
  waitEither_ a b
{-# INLINABLE race_ #-}

-- | Generalized version of 'A.concurrently'.
concurrently :: MonadBaseControl IO m => m a -> m b -> m (a, b)
concurrently left right =
  Safe.withAsync left $ \a ->
  Safe.withAsync right $ \b ->
  waitBoth a b
{-# INLINABLE concurrently #-}

-- | Generalized version of 'A.mapConcurrently'.
mapConcurrently
  :: (Traversable t, MonadBaseControl IO m)
  => (a -> m b)
  -> t a
  -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- | Generalized version of 'A.Concurrently'.
--
-- A value of type @Concurrently b m a@ is an IO-based operation that can be
-- composed with other 'Concurrently' values, using the 'Applicative' and
-- 'Alternative' instances.
--
-- Calling 'runConcurrently' on a value of type @Concurrently b m a@ will
-- execute the IO-based lifted operations it contains concurrently, before
-- delivering the result of type 'a'.
--
-- For example
--
-- > (page1, page2, page3) <- runConcurrently $ (,,)
-- >   <$> Concurrently (getURL "url1")
-- >   <*> Concurrently (getURL "url2")
-- >   <*> Concurrently (getURL "url3")
newtype Concurrently (b :: * -> *) m a = Concurrently { runConcurrently :: m a }

-- NOTE: The phantom type variable @b :: * -> *@ in 'Concurrently' is needed to
-- avoid @UndecidableInstances@ in the following instance declarations.
-- See https://github.com/maoe/lifted-async/issues/4 for alternative
-- implementaions.

instance (b ~ IO, Functor m) => Functor (Concurrently b m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance (b ~ IO, MonadBaseControl b m) => Applicative (Concurrently b m) where
  pure = Concurrently . pure
  Concurrently fs <*> Concurrently as =
    Concurrently $ uncurry ($) <$> concurrently fs as

instance (b ~ IO, MonadBaseControl b m) => Alternative (Concurrently b m) where
  empty = Concurrently . liftBaseWith . const $ forever (threadDelay maxBound)
  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

instance Monad m => Monad (Concurrently b m) where
  return = Concurrently . return
  Concurrently a >>= f = Concurrently $ a >>= runConcurrently . f

sequenceEither :: MonadBaseControl IO m => Either e (StM m a) -> m (Either e a)
sequenceEither = either (return . Left) (liftM Right . restoreM)
