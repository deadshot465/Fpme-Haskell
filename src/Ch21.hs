{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch21 where

import Prelude

import Ch18 (MonadState(..), MonadAsk(..), MonadTell(..), MonadThrow(..))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Bifunctor ( Bifunctor(first) )
import Data.Functor ((<&>))

test :: IO ()
test = do
  putStrLn "String"

newtype StateT s m a = StateT (s -> m (a, s))

runStateT :: StateT s m a -> s -> m (a, s)
runStateT (StateT mf) = mf

instance Functor m => Functor (StateT s m) where
  fmap f (StateT mg) = StateT $ fmap (first f) . mg

instance Monad m => Applicative (StateT s m) where
  (<*>) (StateT mf) (StateT mg) = StateT (\s -> do
    (f, s') <- mf s
    (a, s'') <- mg s'
    pure (f a, s''))
  pure x = StateT $ \s -> pure (x, s)

instance Monad m => Monad (StateT s m) where
  (>>=) (StateT mf) mg = StateT (\s -> do
    (a, s') <- mf s
    runStateT (mg a) s')

instance MonadTrans (StateT s) where
  lift mx = StateT $ \s -> mx <&> (, s)

instance Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure . f

instance MonadAsk r m => MonadAsk r (StateT s m) where
  ask = lift ask

instance MonadTell s m => MonadTell s (StateT s m) where
  tell = lift . tell

instance MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift . throwError

{- liftStateT :: Functor m => m a -> StateT t m a
liftStateT mx = StateT $ \s -> mx <&> (, s) -}