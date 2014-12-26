{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Control.Concurrent.Async.Lifted.Safe
Copyright   : Copyright (C) 2012-2014 Mitsutoshi Aoe
License     : BSD-style (see the file LICENSE)
Maintainer  : Mitsutoshi Aoe <maoe@foldr.in>
Stability   : experimental

This is a wrapped version of "Control.Concurrent.Async" with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.

This module assumes your monad stack to satisfy @'StM' m a ~ a@ for safety.
The safety here means you can't accidentally overwrite monadic effects because
the monad stack doesn't have state in the first place. If your monad stack is
stateful, use @Control.Concurrent.Async.Lifted@ with special care.

#if MIN_VERSION_monad_control(1, 0, 0)
Caveat: Currently due to an implementation restriction, there's no
`A.Concurrently` type and accompanying functions.
#else
Caveat: This module is available only if built with @monad-control >= 1.0.0@.
If you have older @monad-control@, use @Control.Concurrent.Async.Lifted@.
#endif
-}

module Control.Concurrent.Async.Lifted.Safe
  (
#if MIN_VERSION_monad_control(1, 0, 0)
    -- * Asynchronous actions
    A.Async
    -- ** Spawning
  , async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask

    -- ** Spawning with automatic 'cancel'ation
  , withAsync, withAsyncBound, withAsyncOn
  , withAsyncWithUnmask, withAsyncOnWithUnmask

    -- ** Quering 'Async's
  , wait, poll, waitCatch
  , cancel, cancelWith
  , A.asyncThreadId

    -- ** STM operations
  , A.waitSTM, A.pollSTM, A.waitCatchSTM

    -- ** Waiting for multiple 'Async's
  , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
  , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel
  , Unsafe.waitEither_
  , waitBoth

    -- ** Linking
  , Unsafe.link, Unsafe.link2

    -- * Convenient utilities
  , race, race_, concurrently, mapConcurrently
  , Concurrently(..), Pure
#endif
  ) where

#if MIN_VERSION_monad_control(1, 0, 0)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Traversable

import Control.Concurrent.Async (Async)
import Control.Exception.Lifted (SomeException, Exception)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control hiding (restoreM)
import Data.Constraint ((\\), (:-))
import Data.Constraint.Forall (Forall, inst)
import qualified Control.Concurrent.Async as A

import qualified Control.Concurrent.Async.Lifted as Unsafe

-- | Generalized version of 'A.async'.
async :: (MonadBaseControl IO m, StM m a ~ a) => m a -> m (Async a)
async = Unsafe.async

-- | Generalized version of 'A.asyncBound'.
asyncBound :: (MonadBaseControl IO m, StM m a ~ a) => m a -> m (Async a)
asyncBound = Unsafe.asyncBound

-- | Generalized version of 'A.asyncOn'.
asyncOn :: (MonadBaseControl IO m, StM m a ~ a) => Int -> m a -> m (Async a)
asyncOn = Unsafe.asyncOn

-- | Generalized version of 'A.asyncWithUnmask'.
asyncWithUnmask
  :: (MonadBaseControl IO m, StM m a ~ a)
  => ((forall b. m b -> m b) -> m a)
  -> m (Async a)
asyncWithUnmask = Unsafe.asyncWithUnmask

-- | Generalized version of 'A.asyncOnWithUnmask'.
asyncOnWithUnmask
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Int
  -> ((forall b. m b -> m b) -> m a)
  -> m (Async a)
asyncOnWithUnmask = Unsafe.asyncOnWithUnmask

-- | Generalized version of 'A.withAsync'.
withAsync
  :: (MonadBaseControl IO m, StM m a ~ a)
  => m a
  -> (Async a -> m b)
  -> m b
withAsync = Unsafe.withAsync

-- | Generalized version of 'A.withAsyncBound'.
withAsyncBound
  :: (MonadBaseControl IO m, StM m a ~ a)
  => m a
  -> (Async a -> m b)
  -> m b
withAsyncBound = Unsafe.withAsyncBound

-- | Generalized version of 'A.withAsyncOn'.
withAsyncOn
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Int
  -> m a
  -> (Async a -> m b)
  -> m b
withAsyncOn = Unsafe.withAsyncOn

-- | Generalized version of 'A.withAsyncWithUnmask'.
withAsyncWithUnmask
  :: (MonadBaseControl IO m, StM m a ~ a)
  => ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncWithUnmask = Unsafe.withAsyncWithUnmask

-- | Generalized version of 'A.withAsyncOnWithUnmask'.
withAsyncOnWithUnmask
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Int
  -> ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncOnWithUnmask = Unsafe.withAsyncOnWithUnmask

-- | Generalized version of 'A.wait'.
wait :: (MonadBaseControl IO m, StM m a ~ a) => Async a -> m a
wait = Unsafe.wait

-- | Generalized version of 'A.poll'.
poll
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Async a
  -> m (Maybe (Either SomeException a))
poll = Unsafe.poll

-- | Generalized version of 'A.waitCatch'.
waitCatch
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Async a
  -> m (Either SomeException a)
waitCatch = Unsafe.waitCatch

-- | Generalized version of 'A.cancel'.
cancel :: MonadBase IO m => Async a -> m ()
cancel = Unsafe.cancel

-- | Generalized version of 'A.cancelWith'.
cancelWith :: (MonadBase IO m, Exception e) => Async a -> e -> m ()
cancelWith = Unsafe.cancelWith

-- | Generalized version of 'A.waitAny'.
waitAny
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async a] -> m (Async a, a)
waitAny = Unsafe.waitAny

-- | Generalized version of 'A.waitAnyCatch'.
waitAnyCatch
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async a]
  -> m (Async a, Either SomeException a)
waitAnyCatch = Unsafe.waitAnyCatch

-- | Generalized version of 'A.waitAnyCancel'.
waitAnyCancel
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async a]
  -> m (Async a, a)
waitAnyCancel = Unsafe.waitAnyCancel

-- | Generalized version of 'A.waitAnyCatchCancel'.
waitAnyCatchCancel
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async a]
  -> m (Async a, Either SomeException a)
waitAnyCatchCancel = Unsafe.waitAnyCatchCancel

-- | Generalized version of 'A.waitEither'.
waitEither
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async a
  -> Async b
  -> m (Either a b)
waitEither = Unsafe.waitEither

-- | Generalized version of 'A.waitEitherCatch'.
waitEitherCatch
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async a
  -> Async b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch = Unsafe.waitEitherCatch

-- | Generalized version of 'A.waitEitherCancel'.
waitEitherCancel
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async a
  -> Async b
  -> m (Either a b)
waitEitherCancel = Unsafe.waitEitherCancel

-- | Generalized version of 'A.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async a
  -> Async b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel = Unsafe.waitEitherCatchCancel

-- | Generalized version of 'A.waitBoth'.
waitBoth
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async a
  -> Async b
  -> m (a, b)
waitBoth = Unsafe.waitBoth

-- | Generalized version of 'A.race'.
race
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => m a -> m b -> m (Either a b)
race = Unsafe.race

-- | Generalized version of 'A.race_'.
race_
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => m a -> m b -> m ()
race_ = Unsafe.race_

-- | Generalized version of 'A.concurrently'.
concurrently
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => m a -> m b -> m (a, b)
concurrently = Unsafe.concurrently

-- | Generalized version of 'A.mapConcurrently'.
mapConcurrently
  :: (Traversable t, MonadBaseControl IO m, Forall (Pure m))
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
data Concurrently (base :: * -> *) m a where
  Concurrently
    :: Forall (Pure m)
    => { runConcurrently :: m a } -> Concurrently base m a

-- NOTE: The phantom type variable @b :: * -> *@ in 'Concurrently' is needed to
-- avoid @UndecidableInstances@ in the following instance declarations.
-- See https://github.com/maoe/lifted-async/issues/4 for alternative
-- implementaions.

class StM m a ~ a => Pure m a
instance StM m a ~ a => Pure m a

instance (base ~ IO, Functor m) => Functor (Concurrently base m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance (base ~ IO, MonadBaseControl base m, Forall (Pure m)) =>
  Applicative (Concurrently base m) where
    pure = Concurrently . pure
    Concurrently (fs :: m (a -> b)) <*> Concurrently as =
      Concurrently (uncurry ($) <$> concurrently fs as)
        \\ (inst :: Forall (Pure m) :- Pure m a)
        \\ (inst :: Forall (Pure m) :- Pure m (a -> b))

instance (base ~ IO, MonadBaseControl base m, Forall (Pure m)) =>
  Alternative (Concurrently base m) where
    empty = Concurrently . liftBaseWith . const $ forever (threadDelay maxBound)
    Concurrently (as :: m a) <|> Concurrently bs =
      Concurrently (either id id <$> race as bs)
        \\ (inst :: Forall (Pure m) :- Pure m a)
        \\ (inst :: Forall (Pure m) :- Pure m b)

instance (Monad m, Forall (Pure m)) => Monad (Concurrently base m) where
  return = Concurrently . return
  Concurrently a >>= f = Concurrently $ a >>= runConcurrently . f
#endif
