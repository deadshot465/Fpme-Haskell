module Ch11 where

import Prelude hiding (reverse, max, sum)

import Data.List.NonEmpty (NonEmpty(..))

test :: IO ()
test = do
  print $ reverse [10, 20, 30]
  print $ max (-1) 99
  print $ max "aa" "z"
  print $ findMax [37, 311, -1, 2, 84]
  print $ findMax ["a", "bbb", "c"]
  print $ findMax' [37, 311, -1, 2, 84]
  print $ findMax' ["a", "bbb", "c"]
  print $ findMaxNE $ 37 :| [311, -1, 2, 84]
  print $ findMaxNE $ "a" :| ["bbb", "c"]
  print $ findMaxNE' $ 37 :| [311, -1, 2, 84]
  print $ findMaxNE' $ "a" :| ["bbb", "c"]
  print $ sum [1, 2, 3]
  print $ sum' [1, 2, 3]
  print $ sum' [2.5, 3.6, 4,8]
  print $ toList (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  print $ sum' (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []

max :: Ord p => p -> p -> p
max x y | x > y = x
        | otherwise = y

findMax :: Ord a => [a] -> Maybe a
findMax [] = Nothing
findMax l@(x : _) = Just $ go x l
  where
    go acc [] = acc
    go acc (x' : xs') = go (max x' acc) xs'

findMax' :: Ord a => [a] -> Maybe a
findMax' [] = Nothing 
findMax' l@(x : _) = Just $ foldl max x l

findMaxNE :: Ord b => NonEmpty b -> b
findMaxNE (first :| l) = foldl max first l

findMaxNE' :: (Foldable t, Ord a) => t a -> a
findMaxNE' = foldl1 max

sum :: [Integer] -> Integer
sum = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go (x + acc) xs

sum' :: (Foldable f, Num a) => f a -> a
sum' = foldl (+) 0

data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node lt rt) = toList lt <> toList rt

instance Foldable Tree where
  foldr f acc = foldr f acc . toList
  foldl f acc = foldl f acc . toList
  foldMap f = foldMap f . toList