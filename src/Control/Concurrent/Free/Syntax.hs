{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Free.Syntax where

import Control.Concurrent.Free

import Lens.Simple

(>>~=) :: Concurrent f a -> (a -> Seq f b) -> Concurrent f b
x >>~= k = x >>= fmap sequential k

(>>~) :: Concurrent f a -> Seq f b -> Concurrent f b
x >>~ y = x >> sequential y

(~>>=) :: Seq f a -> (a -> Concurrent f b) -> Concurrent f b
x ~>>= k = sequential x >>= k

(~>>) :: Seq f a -> Concurrent f b -> Concurrent f b
x ~>> y = sequential x >> y

(|<*>) :: Par f (a -> b) -> Concurrent f a -> Concurrent f b
x |<*> y = x & _inpar %~ (<*> y ^. _unpar)

(<*>|) :: Concurrent f (a -> b) -> Par f a -> Concurrent f b
x <*>| y = x & _unpar %~ (<*> y ^. _inpar)
