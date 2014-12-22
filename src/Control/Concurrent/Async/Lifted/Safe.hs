{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

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
Caveat: This module becomes useful when used with @monad-control >= 1.0.0@.
If you have older @monad-control@, use @Control.Concurrent.Async.Lifted@.
#endif
-}

module Control.Concurrent.Async.Lifted.Safe
  ( -- * Asynchronous actions
    A.Async
    -- ** Spawning
  , async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask

    -- ** Spawning with automatic 'cancel'ation
  , withAsync, withAsyncBound, withAsyncOn
  , withAsyncWithUnmask, withAsyncOnWithUnmask

    -- ** Quering 'Async's
#if MIN_VERSION_monad_control(1, 0, 0)
  , wait, poll, waitCatch
#endif
  , cancel, cancelWith
  , A.asyncThreadId

    -- ** STM operations
  , A.waitSTM, A.pollSTM, A.waitCatchSTM

#if MIN_VERSION_monad_control(1, 0, 0)
    -- ** Waiting for multiple 'Async's
  , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
  , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel
  , waitEither_
  , waitBoth
#endif

    -- ** Linking
  , link, link2

#if MIN_VERSION_monad_control(1, 0, 0)
    -- * Convenient utilities
  , race, race_, concurrently
#endif
  ) where

import GHC.IO (unsafeUnmask)
import Prelude hiding (mapM)

import Control.Concurrent.Async (Async)
import Control.Exception.Lifted (SomeException, Exception)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control hiding (restoreM)
import qualified Control.Concurrent.Async as A
import qualified Control.Exception.Lifted as E

#if MIN_VERSION_monad_control(1, 0, 0)
import Control.Monad ((>=>), liftM)

import qualified Control.Monad.Trans.Control as MonadControl
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

#if MIN_VERSION_monad_control(1, 0, 0)
-- | Generalized version of 'A.wait'.
wait :: (MonadBaseControl IO m, StM m a ~ a) => Async (StM m a) -> m a
wait = liftBase . A.wait >=> restoreM

-- | Generalized version of 'A.poll'.
poll
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Async (StM m a)
  -> m (Maybe (Either SomeException a))
poll a =
  liftBase (A.poll a) >>=
  maybe (return Nothing) (liftM Just . sequenceEither)

-- | Generalized version of 'A.waitCatch'.
waitCatch
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Async (StM m a)
  -> m (Either SomeException a)
waitCatch a = liftBase (A.waitCatch a) >>= sequenceEither
#endif

-- | Generalized version of 'A.cancel'.
cancel :: MonadBase IO m => Async a -> m ()
cancel = liftBase . A.cancel

-- | Generalized version of 'A.cancelWith'.
cancelWith :: (MonadBase IO m, Exception e) => Async a -> e -> m ()
cancelWith = (liftBase .) . A.cancelWith

#if MIN_VERSION_monad_control(1, 0, 0)
-- | Generalized version of 'A.waitAny'.
waitAny
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async (StM m a)] -> m (Async (StM m a), a)
waitAny as = do
  (a, s) <- liftBase $ A.waitAny as
  r <- restoreM s
  return (a, r)

-- | Generalized version of 'A.waitAnyCatch'.
waitAnyCatch
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async (StM m a)]
  -> m (Async (StM m a), Either SomeException a)
waitAnyCatch as = do
  (a, s) <- liftBase $ A.waitAnyCatch as
  r <- sequenceEither s
  return (a, r)

-- | Generalized version of 'A.waitAnyCancel'.
waitAnyCancel
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async (StM m a)]
  -> m (Async (StM m a), a)
waitAnyCancel as = do
  (a, s) <- liftBase $ A.waitAnyCancel as
  r <- restoreM s
  return (a, r)

-- | Generalized version of 'A.waitAnyCatchCancel'.
waitAnyCatchCancel
  :: (MonadBaseControl IO m, StM m a ~ a)
  => [Async (StM m a)]
  -> m (Async (StM m a), Either SomeException a)
waitAnyCatchCancel as = do
  (a, s) <- liftBase $ A.waitAnyCatchCancel as
  r <- sequenceEither s
  return (a, r)

-- | Generalized version of 'A.waitEither'.
waitEither
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either a b)
waitEither a b =
  liftBase (A.waitEither a b) >>=
  either (liftM Left . restoreM) (liftM Right . restoreM)

-- | Generalized version of 'A.waitEitherCatch'.
waitEitherCatch
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (liftM Left . sequenceEither) (liftM Right . sequenceEither)

-- | Generalized version of 'A.waitEitherCancel'.
waitEitherCancel
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either a b)
waitEitherCancel a b =
  liftBase (A.waitEitherCancel a b) >>=
  either (liftM Left . restoreM) (liftM Right . restoreM)

-- | Generalized version of 'A.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async (StM m a)
  -> Async (StM m b)
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b =
  liftBase (A.waitEitherCatch a b) >>=
  either (liftM Left . sequenceEither) (liftM Right . sequenceEither)

-- | Generalized version of 'A.waitEither_'.
waitEither_
  :: MonadBaseControl IO m
  => Async a
  -> Async b
  -> m ()
waitEither_ a b = liftBase (A.waitEither_ a b)

-- | Generalized version of 'A.waitBoth'.
waitBoth
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => Async (StM m a)
  -> Async (StM m b)
  -> m (a, b)
waitBoth a b = do
  (sa, sb) <- liftBase (A.waitBoth a b)
  ra <- restoreM sa
  rb <- restoreM sb
  return (ra, rb)
{-# INLINABLE waitBoth #-}
#endif

-- | Generalized version of 'A.link'.
link :: MonadBase IO m => Async a -> m ()
link = liftBase . A.link

-- | Generalized version of 'A.link2'.
link2 :: MonadBase IO m => Async a -> Async a -> m ()
link2 = (liftBase .) . A.link2

#if MIN_VERSION_monad_control(1, 0, 0)
-- | Generalized version of 'A.race'.
race
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => m a -> m b -> m (Either a b)
race left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither a b
{-# INLINABLE race #-}

-- | Generalized version of 'A.race_'.
race_ :: MonadBaseControl IO m => m a -> m b -> m ()
race_ left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither_ a b
{-# INLINABLE race_ #-}

-- | Generalized version of 'A.concurrently'.
concurrently
  :: (MonadBaseControl IO m, StM m a ~ a, StM m b ~ b)
  => m a -> m b -> m (a, b)
concurrently left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitBoth a b
{-# INLINABLE concurrently #-}

-- | Generalized version of 'A.mapConcurrently'.
-- mapConcurrently
--   :: (Traversable t, MonadBaseControl IO m, StM m b ~ b)
--   => (a -> m b)
--   -> t a
--   -> m (t b)
-- mapConcurrently f = runConcurrently . traverse (Concurrently . f)

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
-- newtype Concurrently (b :: * -> *) m a = Concurrently { runConcurrently :: m a }
--
-- NOTE: The phantom type variable @b :: * -> *@ in 'Concurrently' is needed to
-- avoid @UndecidableInstances@ in the following instance declarations.
-- See https://github.com/maoe/lifted-async/issues/4 for alternative
-- implementaions.
--
-- instance (b ~ IO, Functor m) => Functor (Concurrently b m) where
--   fmap f (Concurrently a) = Concurrently $ f <$> a
--
-- instance (b ~ IO, MonadBaseControl b m) => Applicative (Concurrently b m) where
--   pure = Concurrently . pure
--   Concurrently fs <*> Concurrently as = undefined
--     -- Concurrently $ uncurry ($) <$> concurrently fs as
--
-- instance (b ~ IO, MonadBaseControl b m) => Alternative (Concurrently b m) where
--   empty = Concurrently . liftBaseWith . const $ forever (threadDelay maxBound)
--   Concurrently as <|> Concurrently bs = undefined
--     -- Concurrently $ either id id <$> race as bs
--
-- instance Monad m => Monad (Concurrently b m) where
--   return = Concurrently . return
--   Concurrently a >>= f = Concurrently $ a >>= runConcurrently . f

sequenceEither
  :: (MonadBaseControl IO m, StM m a ~ a)
  => Either e (StM m a)
  -> m (Either e a)
sequenceEither = either (return . Left) (liftM Right . restoreM)

-- | Type restricted version of `Control.restoreM`.
--
-- The constraint @'StM' m a ~ a@ says that 'restoreM' can be used in stateless
-- monads. It ensures that the user cannot mess up the monadic effects besides
-- IO.
--
-- If you'd like to use this library with a stateful monad, please use
-- 'Control.Concurrent.Async.Lifted.Unsafe' carefully instead.
restoreM :: (MonadBaseControl b m, StM m a ~ a) => StM m a -> m a
restoreM = MonadControl.restoreM
#endif
