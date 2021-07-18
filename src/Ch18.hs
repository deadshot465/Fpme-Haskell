{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Ch18 where

import Prelude

import Data.Bifunctor (first)
import Data.Function ((&))
import Control.Monad ((>=>))

newtype Config = Config { debugModeOn :: Bool } deriving (Show)
type Counter = Int

test :: IO ()
test = do
  print $ runRWS rwsTest RWSResult { r = Config { debugModeOn = True }, w = mempty, s = 0 }

rwsTest :: RWS Config [[Char]] Counter ()
rwsTest = do
  tell ["test the log"]
  tell ["test the log2", "test the log3"]
  config <- ask
  tell ["the config is " <> show config]
  counter <- get
  tell ["old counter is " <> show counter]
  put $ counter + 1
  newCounter <- get
  tell ["new counter is " <> show newCounter]
  pure ()

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

data RWSResult r w s = RWSResult
  { r :: r
  , w :: w
  , s :: s } deriving (Show)

newtype RWS r w s a = RWS (RWSResult r w s -> (a, RWSResult r w s))

instance Functor (RWS r w s) where
  fmap f (RWS g) = RWS (\rws -> g rws & first f)

instance Monoid w => Applicative (RWS r w s) where
  pure x = RWS (\RWSResult { r, s } -> (x, RWSResult { r, w = mempty, s }))
  (<*>) (RWS f) (RWS g) = RWS (\rws -> g rws & \(a, state@RWSResult { w }) -> f state & \(ab, RWSResult { r = r', w = w', s = s' }) -> (ab a, RWSResult { r = r', w = w <> w', s = s' }))
  {- (<*>) (RWS f) (RWS g) = RWS (\rws -> do
    (a, state) <- pure $ g rws
    (ab, state') <- pure $ f rws
    (ab a, state)) -}

instance Monoid w => Monad (RWS r w s) where
  (>>=) (RWS f) g = RWS (\rws -> f rws & \(a, state@RWSResult { w }) -> runRWS (g a) state & \(b, RWSResult { r = r', w = w', s = s' }) -> (b, RWSResult { r = r', w = w <> w', s = s' }))

runRWS :: RWS r w s a -> RWSResult r w s -> (a, RWSResult r w s)
runRWS (RWS rws) = rws

tell :: w -> RWS r w s ()
tell w = RWS (\RWSResult { r, s } -> ((), RWSResult { r, w, s }))

ask :: Monoid w => RWS r w s r
ask = RWS (\RWSResult { r, s } -> (r, RWSResult { r, w = mempty, s }))

get :: Monoid w => RWS r w s s
get = RWS (\RWSResult { r, s } -> (s, RWSResult { r, w = mempty, s }))

put :: Monoid w => s -> RWS r w s ()
put s = RWS (\RWSResult { r } -> ((), RWSResult { r, w = mempty, s }))