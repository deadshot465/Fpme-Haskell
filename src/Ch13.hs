module Ch13 where

import Prelude hiding (Maybe(..), Either(..))

import Data.Bifunctor (Bifunctor(..))
import Data.Text (Text(..), toUpper, unpack, pack)

test :: IO ()
test = do
  print $ (/ 2) <$> Just 10
  print $ (/ 2) <$> Nothing
  print $ (/ 2) <$> (Right 10 :: Either () Double)
  print $ (/ 2) <$> Left "error reason"
  print $ (/ 2) <$> Tuple 10 20
  print $ (/ 2) <$> Triple 10 20 40
  let g x = x * 2
      f x = x * 3
  print $ "Maybe Identity for Nothing: " <> show ((id <$> Nothing) == (Nothing :: Maybe Double))
  print $ "Maybe Identity for Just: " <> show ((id <$> Just 10) == Just 20.0)
  print $ "Maybe Composition for Nothing: " <> show (fmap (g . f) Nothing == (fmap g . fmap f) Nothing)
  print $ "Maybe Composition for Just: " <> show (fmap (g . f) (Just 60) == (fmap g . fmap f) (Just 60))
  print $ second (* 2) $ Left "error reason"
  print $ second (* 2) (Right 10 :: Either () Double)
  print $ first toUpper (Left (pack "error reason") :: Either Text ())
  print $ first toUpper $ Right 10
  print $ second (* 2) $ Tuple 80 40
  print $ first (/ 2) $ Tuple 80 40
  print $ bimap (/ 2) (* 2) $ Tuple 80 40
  print $ second (* 2) $ Triple 99 80 40
  print $ first (/ 2) $ Triple 99 80 40
  print $ bimap (/ 2) (* 2) $ Triple 99 80 40

data Maybe a = Nothing | Just a deriving (Eq, Show)

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

data Either a b = Left a | Right b deriving (Eq, Show)

instance Functor (Either a) where
  fmap _ (Left x) = Left  x
  fmap f (Right x) = Right $ f x

data Tuple a b = Tuple a b deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a $ f b

data Triple a b c = Triple a b c deriving (Eq, Show)

instance Functor (Triple a b) where
  fmap f (Triple a b c) = Triple a b $ f c

instance Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x

instance Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

instance Bifunctor (Triple a) where
  bimap f g (Triple x y z) = Triple x (f y) (g z)