module Ch7a
    ( test
    ) where

import Data.Eq (Eq)
import Data.Ord (Ord(..))
import Text.Show (Show(..))
import Prelude hiding (Maybe(..), Either(..))

data Maybe a = Nothing | Just a deriving (Eq, Ord, Show)

test :: IO ()
test = do
  print $ Just 5 == Just 5
  print $ Just 5 == Just 2
  print $ Just 5 == Nothing
  print $ Nothing == Just 5
  print $ Nothing == (Nothing :: Maybe ())
  putStrLn "------------------"
  print $ Just 1 < Just 5
  print $ Just 5 <= Just 5
  print $ Just 5 > Just 10
  print $ Just 10 >= Just 10
  print $ Just 99 > Nothing
  print $ Just 99 < Nothing
  putStrLn "------------------"
  print $ Just "abc"
  print (Nothing :: Maybe ())
  putStrLn "------------------"
  print (Left "left" :: Either [Char] ())
  print (Right (Just 42) :: Either () (Maybe Int))

{- instance Eq a => Eq (Maybe a) where
  (==) (Just x) (Just y) = x == y
  (==) Nothing Nothing = True
  (==) (Just _) Nothing = False
  (==) Nothing (Just _) = False -}

{- instance Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare (Just x) (Just y) = compare x y
  compare (Just _) Nothing = GT 
  compare Nothing (Just _) = LT -} 

{- instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "(Just " ++ show x ++ ")" -}

data Either a b = Left a | Right b deriving (Eq, Ord, Show)