{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch18 where

import Prelude

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans, lift)

newtype Config = Config { debugModeOn :: Bool } deriving (Show)
type Counter = Int

test :: IO ()
test = do
  print $ runRWS rwsTest RWSResult { r = Config { debugModeOn = True }, w = mempty, s = 0 }

rwsTest :: RWS Config [[Char]] Counter ()
rwsTest = do
  tellRWS ["test the log"]
  tellRWS ["test the log2", "test the log3"]
  config <- askRWS
  tellRWS ["the config is " <> show config]
  counter <- getRWS
  tellRWS ["old counter is " <> show counter]
  putRWS $ counter + 1
  newCounter <- getRWS
  tellRWS ["new counter is " <> show newCounter]
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

newtype MyWriter w a = MyWriter (a, w)

instance Functor (MyWriter w) where
  fmap f (MyWriter (a, w)) = MyWriter (f a, w)

instance Monoid w => Applicative (MyWriter w) where
  (<*>) (MyWriter (f, w)) (MyWriter (a, w')) = MyWriter (f a, w <> w')
  pure x = MyWriter (x, mempty)

instance Monoid w => Monad (MyWriter w) where
  (>>=) (MyWriter (a, w)) f = f a & \(MyWriter (b, w')) -> MyWriter (b, w <> w')

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

tellRWS :: w -> RWS r w s ()
tellRWS w = RWS (\RWSResult { r, s } -> ((), RWSResult { r, w, s }))

askRWS :: Monoid w => RWS r w s r
askRWS = RWS (\RWSResult { r, s } -> (r, RWSResult { r, w = mempty, s }))

getRWS :: Monoid w => RWS r w s s
getRWS = RWS (\RWSResult { r, s } -> (s, RWSResult { r, w = mempty, s }))

putRWS :: Monoid w => s -> RWS r w s ()
putRWS s = RWS (\RWSResult { r } -> ((), RWSResult { r, w = mempty, s }))

newtype WriterT w m a = WriterT (m (a, w))

runWriterT :: WriterT w m a -> m (a, w)
runWriterT (WriterT mx) = mx

instance Functor m => Functor (WriterT w m) where
  fmap f (WriterT mx) = WriterT $ first f <$> mx

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  (<*>) (WriterT mf) (WriterT mx) = WriterT (do
    (x, w) <- mx
    (f, w') <- mf
    pure (f x, w <> w'))
  pure x = WriterT $ pure (x, mempty)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  (>>=) (WriterT mx) f = WriterT (do
    (x, w) <- mx
    (y, w') <- runWriterT $ f x
    pure (y, w <> w'))

newtype ReaderT r m a = ReaderT (r -> m a)

runReaderT :: ReaderT r m a -> r -> m a
runReaderT (ReaderT mf) = mf

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT mg) = ReaderT (fmap f . mg)

instance Monad m => Applicative (ReaderT r m) where
  {- (<*>) (ReaderT mf) (ReaderT mg) = ReaderT (\r -> do
    a <- mg r
    f <- mf r
    pure $ f a) -}
  (<*>) (ReaderT mf) (ReaderT mg) = ReaderT (\r -> mf r <*> mg r)
  --pure = ReaderT . const . pure
  pure = lift . pure

instance Monad m => Monad (ReaderT r m) where
  (>>=) (ReaderT mf) mg = ReaderT (\r -> mf r >>= \a -> runReaderT (mg a) r)

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

class Monad m => MonadAsk r m | m -> r where
  ask :: m r

class Monad m => MonadTell w m | m -> w where
  tell :: w -> m ()

class Monad m => MonadState s m | m -> s where
  state :: (s -> (a, s)) -> m a

class Monad m => MonadThrow e m | m -> e where
  throwError :: e -> m a

class MonadThrow e m => MonadError e m | m -> e where
  catchError :: m a -> (e -> m a) -> m a

instance Monad m => MonadAsk r (ReaderT r m) where
  ask = ReaderT pure

instance MonadTell w m => MonadTell w (ReaderT r m) where
  tell = lift . tell

instance MonadState s m => MonadState s (ReaderT r m) where
  state = lift . state