{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module MonadStack where

import Prelude

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