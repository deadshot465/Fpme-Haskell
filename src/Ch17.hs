module Ch17 where

import Prelude hiding (Maybe(..), Either(..))

import Data.Bifunctor (Bifunctor(..))

test :: IO ()
test = do
  print $ (+) <$> Just 21 <*> Just 21
  print $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  print $ pure (+) <*> Just 17 <*> Just 25
  print $ ((.) <$> pure id <*> pure id <*> pure 1) == (pure id <*> (pure id <*> pure 1) :: Either () Int)
  print $ (pure id <*> pure 1) == (pure 1 :: Either () Int)
  print $ pure (negate 1) == (pure negate <*> pure 1 :: Either () Int)
  print $ (pure negate <*> pure 1) == (pure (\x -> x $ 1) <*> pure negate :: Either () Int)

data Maybe a = Nothing | Just a deriving (Eq, Show)

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
  pure = Just
  (<*>) Nothing _ = Nothing
  (<*>) (Just f) x = f <$> x

data Either a b = Left a | Right b deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right $ f x

instance Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x

instance Applicative (Either a) where
  pure = Right
  (<*>) (Left y) _ = Left y
  (<*>) (Right f) x = f <$> x