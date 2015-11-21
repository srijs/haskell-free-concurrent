{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Free.Par
  ( Par
  , liftPar
  , hoistPar, retractPar, foldPar
  , foldParM, retractParIO, foldParIO
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (SomeException(..), try, throwIO)
import Control.Monad (join)

data Par f a where
  Pure :: a -> Par f a
  Apply :: f a -> Par f (a -> b) -> Par f b

instance Functor (Par f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Apply x y) = Apply x (pure (fmap f) <*> y)

instance Applicative (Par f) where
  pure = Pure
  Pure f <*> x = fmap f x
  Apply x y <*> z = Apply x (flip <$> y <*> z)

liftPar :: f a -> Par f a
liftPar x = Apply x (Pure id)

hoistPar :: (forall x. f x -> g x) -> Par f a -> Par g a
hoistPar t (Pure a) = Pure a
hoistPar t (Apply x y) = Apply (t x) (hoistPar t y)

retractPar :: Applicative f => Par f a -> f a
retractPar (Pure a) = pure a
retractPar (Apply x y) = retractPar y <*> x

foldPar :: Applicative g => (forall x. f x -> g x) -> Par f a -> g a
foldPar t = retractPar . hoistPar t

foldParM :: Monad m => (forall x. f x -> m (m x)) -> Par f a -> m a
foldParM run (Pure a) = return a
foldParM run (Apply x y) = run x >>= \z -> foldParM run y <*> z

retractParIO :: Par IO a -> IO a
retractParIO = foldParM $ \action -> do
  v <- newEmptyMVar
  forkIO $ try action >>= putMVar v
  return $ do
    r <- takeMVar v
    case r of
      Left (SomeException e) -> throwIO e
      Right a -> return a

foldParIO :: (forall x. f x -> IO x) -> Par f a -> IO a
foldParIO t = retractParIO . hoistPar t
