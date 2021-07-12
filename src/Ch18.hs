{-# LANGUAGE TupleSections #-}
module Ch18 where

import Prelude

import Data.Bifunctor (first)
import Data.Function ((&))
import Control.Monad ((>=>))

newtype MyReader r a = MyReader (r -> a)

instance Functor (MyReader r) where
  fmap f (MyReader x) = MyReader (f . x)

instance Applicative (MyReader r) where
  pure = MyReader . const
  (<*>) (MyReader f) (MyReader x) = MyReader (\r -> f r $ x r)

instance Monad (MyReader r) where
  (>>=) (MyReader x) f = MyReader (\r -> runMyReader (f $ x r) r)

runMyReader :: MyReader r a -> r -> a
runMyReader (MyReader f) = f

newtype State s a = State (s -> (a, s))

instance Functor (State s) where
  fmap f (State fx) = State (\s -> fx s & first f)

instance Applicative (State s) where
  pure x = State (x,)
  (<*>) (State f) (State fx) = State (\s -> f s & \(g, s') -> fx s' & first g)

instance Monad (State s) where
  (>>=) (State ff) f = State (\s -> ff s & \(a, s') -> getState (f a) s')

getState :: State s a -> s -> (a, s)
getState (State s) = s