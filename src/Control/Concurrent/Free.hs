{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Free where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (SomeException(..), try, throwIO)
import Control.Monad (join)

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

liftF :: f a -> F f a
liftF x = Ap x id (Pure id)

hoist :: (forall a. f a -> g a) -> F f a -> F g a
hoist f (Pure a) = Pure a
hoist f (Ap x g y) = Ap (f x) g (hoist f y)
hoist f (Join x) = Join (hoist f (fmap (hoist f) x))

retractA :: Applicative f => F f a -> Maybe (f a)
retractA (Pure a) = Just (pure a)
retractA (Ap x g y) = (\z -> z <*> fmap g x) <$> retractA y
retractA (Join x) = Nothing

retractM :: Monad f => F f a -> f a
retractM (Pure a) = pure a
retractM (Ap x g y) = retractM y >>= \z -> z <$> g <$> x
retractM (Join x) = join . retractM $ fmap retractM x

retractConcurrentM :: Monad f => (forall x. f x -> f (f x)) -> F f a -> f a
retractConcurrentM run (Pure x) = return x
retractConcurrentM run (Ap x g y) = do
  v <- run x
  f <- retractConcurrentM run y
  f . g <$> v
retractConcurrentM run (Join x) = do
  y <- retractConcurrentM run x
  retractConcurrentM run y

retractConcurrentIO :: F IO a -> IO a
retractConcurrentIO = retractConcurrentM $ \action -> do
  v <- newEmptyMVar
  forkIO $ try action >>= putMVar v
  return $ do
    r <- takeMVar v
    case r of
      Left (SomeException e) -> throwIO e
      Right a -> return a

foldA :: Applicative g => (forall x. f x -> g x) -> F f a -> Maybe (g a)
foldA f = retractA . hoist f

foldM :: Monad m => (forall x. f x -> m x) -> F f a -> m a
foldM f = retractM . hoist f

foldConcurrentIO :: (forall x. f x -> IO x) -> F f a -> IO a
foldConcurrentIO f = retractConcurrentIO . hoist f
