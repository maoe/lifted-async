{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Control.Concurrent.Async.Lifted.Safe
Copyright   : Copyright (C) 2012-2015 Mitsutoshi Aoe
License     : BSD-style (see the file LICENSE)
Maintainer  : Mitsutoshi Aoe <maoe@foldr.in>
Stability   : experimental

This is a safe variant of @Control.Concurrent.Async.Lifted@.

This module assumes your monad stack to satisfy @'StM' m a ~ a@ so you can't
mess up monadic effects. If your monad stack is stateful, use
@Control.Concurrent.Async.Lifted@ with special care.
-}

module Control.Concurrent.Async.Lifted.Safe
#if MIN_VERSION_monad_control(1, 0, 0)
  (
    -- * Asynchronous actions
    A.Async

  , Pure
  , Forall
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

#if MIN_VERSION_async(2, 1, 0)
    -- ** Waiting for multiple 'Async's in STM
  , A.waitAnySTM
  , A.waitAnyCatchSTM
  , A.waitEitherSTM
  , A.waitEitherCatchSTM
  , A.waitEitherSTM_
  , A.waitBothSTM
#endif

    -- ** Linking
  , Unsafe.link, Unsafe.link2

    -- * Convenient utilities
  , race, race_, concurrently, mapConcurrently
  , Concurrently(..)


  )
#else
{-# WARNING
  "This module is available only if built with @monad-control >= 1.0.0@.\
  If you have an older version of @monad-control@, use\
  @Control.Concurrent.Async.Lifted@ instead."
  #-}
#endif
  where

#if MIN_VERSION_monad_control(1, 0, 0)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad

import Control.Concurrent.Async (Async)
import Control.Exception.Lifted (SomeException, Exception)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control hiding (restoreM)
import Data.Constraint ((\\), (:-))
import Data.Constraint.Forall (Forall, inst)
import qualified Control.Concurrent.Async as A

import qualified Control.Concurrent.Async.Lifted as Unsafe

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
#if !MIN_VERSION_base(4, 8, 0)
import Data.Monoid (Monoid(mappend, mempty))
#elif MIN_VERSION_base(4, 9, 0)
import Data.Semigroup (Semigroup((<>)))
#endif

-- | Generalized version of 'A.async'.
async
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => m a -> m (Async a)
async = Unsafe.async
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.asyncBound'.
asyncBound
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => m a -> m (Async a)
asyncBound = Unsafe.asyncBound
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.asyncOn'.
asyncOn
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => Int -> m a -> m (Async a)
asyncOn cpu m = Unsafe.asyncOn cpu m
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.asyncWithUnmask'.
asyncWithUnmask
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => ((forall b. m b -> m b) -> m a)
  -> m (Async a)
asyncWithUnmask restore = Unsafe.asyncWithUnmask restore
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.asyncOnWithUnmask'.
asyncOnWithUnmask
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => Int
  -> ((forall b. m b -> m b) -> m a)
  -> m (Async a)
asyncOnWithUnmask cpu restore = Unsafe.asyncOnWithUnmask cpu restore
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.withAsync'.
withAsync
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => m a
  -> (Async a -> m b)
  -> m b
withAsync = Unsafe.withAsync
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.withAsyncBound'.
withAsyncBound
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => m a
  -> (Async a -> m b)
  -> m b
withAsyncBound = Unsafe.withAsyncBound
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.withAsyncOn'.
withAsyncOn
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => Int
  -> m a
  -> (Async a -> m b)
  -> m b
withAsyncOn = Unsafe.withAsyncOn
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.withAsyncWithUnmask'.
withAsyncWithUnmask
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncWithUnmask restore = Unsafe.withAsyncWithUnmask restore
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.withAsyncOnWithUnmask'.
withAsyncOnWithUnmask
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => Int
  -> ((forall c. m c -> m c) -> m a)
  -> (Async a -> m b)
  -> m b
withAsyncOnWithUnmask cpu restore = Unsafe.withAsyncOnWithUnmask cpu restore
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.wait'.
wait
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => Async a -> m a
wait = Unsafe.wait
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.poll'.
poll
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => Async a
  -> m (Maybe (Either SomeException a))
poll = Unsafe.poll
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.waitCatch'.
waitCatch
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => Async a
  -> m (Either SomeException a)
waitCatch = Unsafe.waitCatch
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.cancel'.
cancel :: MonadBase IO m => Async a -> m ()
cancel = Unsafe.cancel

-- | Generalized version of 'A.cancelWith'.
cancelWith :: (MonadBase IO m, Exception e) => Async a -> e -> m ()
cancelWith = Unsafe.cancelWith

-- | Generalized version of 'A.waitAny'.
waitAny
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => [Async a] -> m (Async a, a)
waitAny = Unsafe.waitAny
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.waitAnyCatch'.
waitAnyCatch
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => [Async a]
  -> m (Async a, Either SomeException a)
waitAnyCatch = Unsafe.waitAnyCatch
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.waitAnyCancel'.
waitAnyCancel
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => [Async a]
  -> m (Async a, a)
waitAnyCancel = Unsafe.waitAnyCancel
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.waitAnyCatchCancel'.
waitAnyCatchCancel
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m))
  => [Async a]
  -> m (Async a, Either SomeException a)
waitAnyCatchCancel = Unsafe.waitAnyCatchCancel
  \\ (inst :: Forall (Pure m) :- Pure m a)

-- | Generalized version of 'A.waitEither'.
waitEither
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => Async a
  -> Async b
  -> m (Either a b)
waitEither = Unsafe.waitEither
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.waitEitherCatch'.
waitEitherCatch
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => Async a
  -> Async b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch = Unsafe.waitEitherCatch
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.waitEitherCancel'.
waitEitherCancel
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => Async a
  -> Async b
  -> m (Either a b)
waitEitherCancel = Unsafe.waitEitherCancel
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => Async a
  -> Async b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel = Unsafe.waitEitherCatchCancel
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.waitBoth'.
waitBoth
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => Async a
  -> Async b
  -> m (a, b)
waitBoth = Unsafe.waitBoth
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.race'.
race
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => m a -> m b -> m (Either a b)
race = Unsafe.race
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.race_'.
race_
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => m a -> m b -> m ()
race_ = Unsafe.race_
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.concurrently'.
concurrently
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m))
  => m a -> m b -> m (a, b)
concurrently = Unsafe.concurrently
  \\ (inst :: Forall (Pure m) :- Pure m a)
  \\ (inst :: Forall (Pure m) :- Pure m b)

-- | Generalized version of 'A.mapConcurrently'.
mapConcurrently
  :: (Traversable t, MonadBaseControl IO m, Forall (Pure m))
  => (a -> m b)
  -> t a
  -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

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
data Concurrently m a where
  Concurrently
    :: Forall (Pure m) => { runConcurrently :: m a } -> Concurrently m a

-- | Most of the functions in this module have @'Forall' ('Pure' m)@ in their
-- constraints, which means they require the monad 'm' satisfies
-- @'StM' m a ~ a@ for all 'a'.
class StM m a ~ a => Pure m a
instance StM m a ~ a => Pure m a

instance Functor m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance (MonadBaseControl IO m, Forall (Pure m)) =>
  Applicative (Concurrently m) where
    pure = Concurrently . pure
    Concurrently (fs :: m (a -> b)) <*> Concurrently as =
      Concurrently (uncurry ($) <$> concurrently fs as)
        \\ (inst :: Forall (Pure m) :- Pure m a)
        \\ (inst :: Forall (Pure m) :- Pure m (a -> b))

instance (MonadBaseControl IO m, Forall (Pure m)) =>
  Alternative (Concurrently m) where
    empty = Concurrently $ liftBaseWith $ const (forever $ threadDelay maxBound)
    Concurrently (as :: m a) <|> Concurrently bs =
      Concurrently (either id id <$> race as bs)
        \\ (inst :: Forall (Pure m) :- Pure m a)
        \\ (inst :: Forall (Pure m) :- Pure m b)

#if MIN_VERSION_base(4, 9, 0)
instance (MonadBaseControl IO m, Semigroup a, Forall (Pure m)) =>
  Semigroup (Concurrently m a) where
    (<>) = liftA2 (<>)

instance (MonadBaseControl IO m, Semigroup a, Monoid a, Forall (Pure m)) =>
  Monoid (Concurrently m a) where
    mempty = pure mempty
    mappend = (<>)
#else
instance (MonadBaseControl IO m, Monoid a, Forall (Pure m)) =>
  Monoid (Concurrently m a) where
    mempty = pure mempty
    mappend = liftA2 mappend
#endif

#endif
