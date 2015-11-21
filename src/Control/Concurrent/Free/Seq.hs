{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Free.Seq
  ( Seq
  , liftSeq
  , hoistSeq, retractSeq, foldSeq
  ) where

import Control.Monad (ap, (>=>))

data Seq f a where
  Pure :: a -> Seq f a
  Roll :: f a -> (a -> Seq f b) -> Seq f b

instance Functor (Seq f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Roll x y) = Roll x (fmap (fmap f) y)

instance Applicative (Seq f) where
  pure = return
  x <*> y = ap x y

instance Monad (Seq f) where
  return = Pure
  Pure a >>= k = k a
  Roll x y >>= k = Roll x (y >=> k)

liftSeq :: f a -> Seq f a
liftSeq x = Roll x Pure

hoistSeq :: (forall x. f x -> g x) -> Seq f a -> Seq g a
hoistSeq t (Pure a) = Pure a
hoistSeq t (Roll x y) = Roll (t x) (fmap (hoistSeq t) y)

retractSeq :: Monad f => Seq f a -> f a
retractSeq (Pure a) = return a
retractSeq (Roll x y) = x >>= fmap retractSeq y

foldSeq :: Monad g => (forall x. f x -> g x) -> Seq f a -> g a
foldSeq t = retractSeq . hoistSeq t
