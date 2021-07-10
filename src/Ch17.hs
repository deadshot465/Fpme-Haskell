{-# LANGUAGE DeriveFunctor #-}
module Ch17 where

import Prelude hiding (Maybe(..), Either(..))

import Data.Bifunctor (Bifunctor(..))
import Data.Text (Text(..), pack)

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

newtype Validation err result = Validation (Either err result) deriving (Eq, Show, Ord, Functor)

instance Bifunctor Validation where
  bimap f _ (Validation (Left x)) = Validation $ Left $ f x
  bimap _ g (Validation (Right x)) = Validation $ Right $ g x

instance Semigroup err => Applicative (Validation err) where
  pure = Validation . Right
  (<*>) (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left $ err1 <> err2
  (<*>) (Validation (Left err)) _ = Validation $ Left err
  (<*>) (Validation (Right f)) x = f <$> x

newtype Age = Age Int deriving (Eq, Show, Ord)

data FamilyAges = FamilyAges { fatherAge :: Age, motherAge :: Age, childAge :: Age } deriving (Eq, Show, Ord)
data FamilyNames = FamilyNames { fatherName :: Text, motherName :: Text, childName :: Text } deriving (Eq, Show, Ord)

newtype LowerAge = LowerAge Int deriving (Eq, Show, Ord)
newtype UpperAge = UpperAge Int deriving (Eq, Show, Ord)

validateAges :: LowerAge -> UpperAge -> Age -> [Char] -> Validation [Text] Age
validateAges (LowerAge low) (UpperAge high) actualAge@(Age age) who | age < low = Validation $ Left [pack $ who <> " is too young."]
                                                                    | age > high = Validation $ Left [pack $ who <> " is too old."]
                                                                    | otherwise = Validation $ Right actualAge

createAges :: FamilyAges -> Validation [Text] FamilyAges
createAges FamilyAges { fatherAge = fa, motherAge = ma, childAge = ca } =
  (\x y z -> FamilyAges { fatherAge = x, motherAge = y, childAge = z }) <$>
  validateAges (LowerAge 1) (UpperAge 18) fa "Father" <*>
  validateAges (LowerAge 1) (UpperAge 18) ma "Mother" <*>
  validateAges (LowerAge 1) (UpperAge 18) ca "Child"