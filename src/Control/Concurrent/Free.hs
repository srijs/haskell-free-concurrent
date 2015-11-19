{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Free where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (SomeException(..), try, throwIO)
import Control.Monad (join)

-- | The combination of a free functor, a free applicative functor,
--   and free monad over @f@.
--
--   The semantics of the 'Functor', 'Applicative' and 'Monad' instances
--   are such that it tries to pick the lowest possible abstraction to
--   perform the operation.
--
--   This means that if a computation is constructed using 'fmap', 'pure'
--   and '<*>', it can be parallelised up until the point where the first
--   monadic 'join' sits.
data F f a where
  Pure :: a -> F f a
  Ap   :: f x -> (x -> a) -> F f (a -> b) -> F f b
  Join :: F f (F f a) -> F f a

instance Functor (F f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap x g y) = Ap x g (fmap f <$> y)
  fmap f (Join x) = Join (fmap f <$> x)

instance Applicative (F f) where
  pure = Pure
  Pure f <*> y = fmap f y
  Ap x g y <*> z = Ap x g (flip <$> y <*> z)
  Join x <*> y = Join ((\z -> z <*> y) <$> x)

instance Monad (F f) where
  return = pure
  Pure x >>= f = f x
  x >>= f = Join (fmap f x)
  x >> y = x *> y

-- | Lifts an @f a@ into a @F f a@.
liftF :: f a -> F f a
liftF x = Ap x id (Pure id)

-- | Given a natural transformation from @f@ to @g@ this gives a monoidal natural transformation from @F f@ to @F g@.
hoist :: (forall a. f a -> g a) -> F f a -> F g a
hoist f (Pure a) = Pure a
hoist f (Ap x g y) = Ap (f x) g (hoist f y)
hoist f (Join x) = Join (hoist f (fmap (hoist f) x))

-- | Partially interprets the free monad over @f@ using the semantics for 'pure' and '<*>' given by the 'Applicative' instance for @f@. If it encounters a monadic join, the result is 'Nothing'.
retractA :: Applicative f => F f a -> Maybe (f a)
retractA (Pure a) = Just (pure a)
retractA (Ap x g y) = (\z -> z <*> fmap g x) <$> retractA y
retractA (Join x) = Nothing

-- | Interprets the free monad over @f@ using the semantics for 'return' and '>>=' given by the 'Monad' instance for @f@.
retractM :: Monad f => F f a -> f a
retractM (Pure a) = pure a
retractM (Ap x g y) = retractM y >>= \z -> z <$> g <$> x
retractM (Join x) = join . retractM $ fmap retractM x

-- | Interprets the free monad over @f@ using the
--   transformation from @f@ to @m m@.
--
--   The semantics of the concurrency are given by the transformation,
--   which produces a result that is unwrapped in two stages:
--   The first monadic layer should spawn the concurrent action,
--   and reveal the second layer, which should block
--   until the spawned action has returned with a result.
foldConcurrentM :: Monad m => (forall x. f x -> m (m x)) -> F f a -> m a
foldConcurrentM run (Pure x) = return x
foldConcurrentM run (Ap x g y) = do
  v <- run x
  f <- foldConcurrentM run y
  f . g <$> v
foldConcurrentM run (Join x) = do
  y <- foldConcurrentM run x
  foldConcurrentM run y

-- | Interprets the free monad over 'IO' using concurrent semantics, meaning multiple actions may run in parallel.
retractConcurrentIO :: F IO a -> IO a
retractConcurrentIO = foldConcurrentM $ \action -> do
  v <- newEmptyMVar
  forkIO $ try action >>= putMVar v
  return $ do
    r <- takeMVar v
    case r of
      Left (SomeException e) -> throwIO e
      Right a -> return a

-- | Given a natural transformation from @f@ to @g@, this gives a partial monoidal natural transformation from @F f@ to @g@.
foldA :: Applicative g => (forall x. f x -> g x) -> F f a -> Maybe (g a)
foldA f = retractA . hoist f

-- | Given a natural transformation from @f@ to @g@, this gives a canonical monoidal natural transformation from @F f@ to @g@.
foldM :: Monad m => (forall x. f x -> m x) -> F f a -> m a
foldM f = retractM . hoist f

-- | Given a natural transformation from @f@ to 'IO', this gives a natural transformation from @F f@ to @IO@ where the actions may run concurrently.
foldConcurrentIO :: (forall x. f x -> IO x) -> F f a -> IO a
foldConcurrentIO f = retractConcurrentIO . hoist f
