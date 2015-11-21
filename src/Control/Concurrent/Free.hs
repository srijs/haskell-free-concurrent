{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Free
  ( module Control.Concurrent.Free.Seq
  , module Control.Concurrent.Free.Par
  , Concurrent, lift, hoist
  , Interpreter(..), interpret
  , retract, fold
  , retractConcurrentIO, foldConcurrentIO
  , sequential, intoSeq, _unseq, _inseq
  , parallel, intoPar, _unpar, _inpar
  ) where

import Control.Concurrent.Free.Seq
import Control.Concurrent.Free.Par
import Control.Monad (ap)

import Lens.Simple

-- | The combination of a free applicative functor and free monad over @f@.
data Concurrent f a where
  Lift :: f a -> Concurrent f a
  Seq :: Seq (Concurrent f) a -> Concurrent f a
  Par :: Par (Concurrent f) a -> Concurrent f a

instance Functor (Concurrent f) where
  fmap f = _unseq %~ fmap f

instance Applicative (Concurrent f) where
  pure = return
  (<*>) = ap

instance Monad (Concurrent f) where
  return = Seq . pure
  x >>= k = x & _unseq %~ (>>= intoSeq . k)
  x >> y = x & _unseq %~ (>> intoSeq y)

lift :: f a -> Concurrent f a
lift = Lift

hoist :: (forall x. f x -> g x) -> Concurrent f a -> Concurrent g a
hoist t (Lift x) = Lift (t x)
hoist t (Seq x) = Seq (hoistSeq (hoist t) x)
hoist t (Par x) = Par (hoistPar (hoist t) x)

data Interpreter f = Interpreter
  { runSeq :: forall x. Seq f x -> f x
  , runPar :: forall x. Par f x -> f x
  }

interpret :: Interpreter f -> Concurrent f a -> f a
interpret int (Lift x) = x
interpret int (Par x) = runPar int $ hoistPar (interpret int) x
interpret int (Seq x) = runSeq int $ hoistSeq (interpret int) x

retract :: Monad f => Concurrent f a -> f a
retract = interpret (Interpreter retractSeq retractPar)

retractConcurrentIO :: Concurrent IO a -> IO a
retractConcurrentIO = interpret (Interpreter retractSeq retractParIO)

fold :: Monad g => (forall x. f x -> g x) -> Concurrent f a -> g a
fold t = retract . hoist t

foldConcurrentIO :: (forall x. f x -> IO x) -> Concurrent f a -> IO a
foldConcurrentIO t = retractConcurrentIO . hoist t

_unseq :: Lens (Concurrent f a) (Concurrent f b) (Seq (Concurrent f) a) (Seq (Concurrent f) b)
_unseq = iso intoSeq Seq

_inseq :: Lens (Seq f a) (Concurrent f b) (Seq (Concurrent f) a) (Seq (Concurrent f) b)
_inseq = iso (hoistSeq lift) Seq

intoSeq :: Concurrent f a -> Seq (Concurrent f) a
intoSeq (Seq x) = x
intoSeq x = liftSeq x

sequential :: Seq f a -> Concurrent f a
sequential = _inseq %~ id

_unpar :: Lens (Concurrent f a) (Concurrent f b) (Par (Concurrent f) a) (Par (Concurrent f) b)
_unpar = iso intoPar Par

_inpar :: Lens (Par f a) (Concurrent f b) (Par (Concurrent f) a) (Par (Concurrent f) b)
_inpar = iso (hoistPar lift) Par

parallel :: Par f a -> Concurrent f a
parallel = _inpar %~ id

intoPar :: Concurrent f a -> Par (Concurrent f) a
intoPar (Par x) = x
intoPar x = liftPar x
