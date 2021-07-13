{-# LANGUAGE DeriveFunctor #-}
module Ch19 where

import Prelude hiding (Maybe(..), Either(..))

test :: IO ()
test = do
  print $ Just (* 10) <*> Just 20
  print $ Just (* 10) <*> pure 20
  print $ Just 20 >>= pure . (* 10)
  print $ do
    x <- Just 20
    let y = x * 10
    pure y
  print $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42
  print $ do
    _ <- Just 20
    y <- Nothing
    pure $ y + 42
  putStrLn "-------------------------------------"
  print $ Right (* 10) <*> (Right 20 :: Either () Int)
  print $ Right (* 10) <*> (pure 20 :: Either () Int)
  print $ (Right 20 :: Either () Int) >>= pure . (* 10)
  print $ do
    x <- (Right 20 :: Either () Int)
    let y = x * 10
    pure y
  print $ Right 20 >>= const (Left "error") >>= \y -> Right $ y * 10
  print $ do
    _ <- Right 20 :: Either [Char] Int
    y <- Left "error"
    pure $ y * 10

data Maybe a = Nothing | Just a deriving (Eq, Show, Ord)

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
  pure = Just
  (<*>) Nothing _ = Nothing
  (<*>) (Just f) x = f <$> x

instance Monad Maybe where
  (>>=) Nothing _ = Nothing
  (>>=) (Just x) f = f x

data Either a b = Left a | Right b deriving (Eq, Ord, Show, Functor)

instance Applicative (Either a) where
  pure = Right
  (<*>) (Left x) _ = Left x
  (<*>) (Right f) x = f <$> x

instance Monad (Either a) where
  (>>=) (Left x) _ = Left x
  (>>=) (Right f) x = x f