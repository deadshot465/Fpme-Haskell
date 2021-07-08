module Ch15 where

import Prelude hiding (odd)

import Data.Bits ((.&.), Bits(..))
import Data.Functor.Contravariant (Contravariant(..), contramap, (>$<))
import Data.Profunctor (Profunctor(..), dimap)

test :: IO ()
test = do
  print $ odd (0 :: Int)
  print $ odd (1 :: Int)
  putStrLn "------------------------------------"
  print $ runPredicate (Predicate odd) (10 :: Int)
  print $ runPredicate (Predicate odd) (11 :: Int)
  putStrLn "------------------------------------"
  print $ runPredicate (contramap (+ 1) (Predicate odd)) (10 :: Int)
  print $ runPredicate (contramap (+ 2) (Predicate odd)) (10 :: Int)
  print $ runPredicate ((+ 1) >$< Predicate odd)  (10 :: Int)
  print $ runPredicate ((+ 2) >$< Predicate odd)  (10 :: Int)
  print $ runFoldL addr [1, 2, 3]
  print $ runFoldL addr [1.0, 2.0, 3.0]
  print $ runFoldL sizer ["This", "is", "the", "test"]

odd :: (Data.Bits.Bits a, Num a) => a -> Bool
odd x = x .&. 1 == 1

newtype Predicate a = Predicate (a -> Bool)

runPredicate :: Predicate a -> a -> Bool
runPredicate (Predicate f) = f

{-
  contramap :: forall f a b. Contravariant f => (a -> b) -> f b -> f a
  f :: a -> b
  b :: b -> Bool 
-}
instance Contravariant Predicate where
  contramap f (Predicate g) = Predicate (g . f)

data Moore s a b = Moore s (s -> b) (s -> a -> s)

addr :: Num a => Moore a a a
addr = Moore 0 id (+)

{-
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d
  f :: c -> a
  g :: b -> d
  output :: s -> b
  transition s :: a -> s
-}
instance Profunctor (Moore a) where
  dimap f g (Moore s output transition) = Moore s (g . output) (\s -> transition s . f)

runFoldL :: Foldable t => Moore b a c -> t a -> c
runFoldL (Moore s output acc) = output . foldl acc s

sizer :: Moore Int [a] [Char]
sizer = dimap length (\x -> "Size is " <> show x) addr