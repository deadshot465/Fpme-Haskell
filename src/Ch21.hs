{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch21 where

import Prelude

import Control.Arrow ((>>>))
import Control.Monad (when, void)
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W

import Data.Bifunctor ( Bifunctor(first) )
import Data.Functor ((<&>))
--import Data.Functor.Identity ( Identity )
import MonadStack
    ( MonadError(..),
      MonadThrow(..),
      MonadState(..),
      MonadTell(..),
      MonadAsk(..) )

test :: IO ()
test = do
  result1 <- runApp 0 app
  print result1
  result2 <- runApp 99 app
  print result2

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

instance MonadError e m => MonadError e (StateT s m) where
  catchError (StateT mf) g = StateT $ \s -> catchError (mf s) (\e -> runStateT (g e) s)

type AppStack e w s a = E.ExceptT e (W.WriterT w (S.StateT s IO)) a

type AppM = AppStack [Char] [Char] Int ()

type StackResult = ((Either [Char] (), [Char]), Int)

data AppEffects = AppEffects
  { log :: [Char]
  , state :: Int
  , result :: Maybe ()
  } deriving (Show)

type AppResult = (Maybe String, AppEffects)

runApp :: Int -> AppM -> IO AppResult
runApp st = (results <$>) . flip S.runStateT st . W.runWriterT . E.runExceptT

{- runApp' :: s -> ExceptT e (WriterT w (S.StateT s m)) a -> m ((Either e a, w), s)
runApp' st = runExceptT >>> runWriterT >>> flip S.runStateT st -}

{- test' :: s -> S.StateT s m a -> m (a, s)
test' = flip S.runStateT -}

results :: StackResult -> AppResult
results ((Left err, l), s) = (Just err, AppEffects { Ch21.log = l, Ch21.state = s, result = Nothing })
results ((Right result, l), s) = (Nothing, AppEffects { Ch21.log = l, Ch21.state = s, result = Just result })

app :: AppM
app = do
  lift $ log' "Starting the app..."
  n <- lift $ lift S.get
  when (n == 0) $ void $ E.throwE "We cannot have a 0 state."
  lift $ lift $ S.put $ n + 1
  lift $ log' "Incremented State."
  pure ()

log' :: Monad m => [Char] -> W.WriterT [Char] m ()
log' s = W.tell $ s <> "\n"