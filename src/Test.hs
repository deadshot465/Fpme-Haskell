module Test where

import Prelude

import Control.Monad ((>=>))
import Data.Text ( Text, pack )

newtype MyFunc e a = MyFunc (Int -> Either e (Text, Text, a))

multiplyTwo :: Num a => a -> a
multiplyTwo x = x * 2

instance Functor (MyFunc e) where
  fmap f (MyFunc fx) = MyFunc (fmap (fmap f) . fx)

instance Applicative (MyFunc e) where
  pure x = MyFunc (\i -> Right (pack "", pack "", x))
  {- (<*>) (MyFunc f) (MyFunc g) = MyFunc (\i -> case g i of
    Left e -> Left e
    Right (s1, s2, x) -> case f i of
      Left e' -> Left e'
      Right (s3, s4, x') -> Right (s1 <> s3, s2 <> s4, x' x)) -}
  (<*>) (MyFunc f) (MyFunc g) = MyFunc (\i -> do
    (s1, s2, x) <- g i
    (s3, s4, x') <- f i
    pure (s1 <> s3, s2 <> s4, x' x))

instance Monad (MyFunc e) where
  (>>=) (MyFunc f) g = MyFunc (\i -> f i >>= \(s1, s2, x) -> getMyFunc (g x) i)

getMyFunc :: MyFunc e a -> Int -> Either e (Text, Text, a)
getMyFunc (MyFunc f) = f

string = MyFunc (\i -> Right (pack $ show i, pack $ show i, i))


twoStrings = (,) <$> string <*> string


threeStrings = (\x y z -> x + y + z) <$> string <*> string <*> string

threeStrings' = string >>= \s -> string >>= \s' -> string >>= \s'' -> pure [s, s', s'']