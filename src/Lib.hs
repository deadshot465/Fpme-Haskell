module Lib
    ( test
    ) where

import Prelude hiding (flip, const, null, length, head, tail, last, init, uncons, reverse, concat, filter)

import Control.Arrow ((<<<))

test :: IO ()
test = do
    print (flip const 1 2)
    print $ null []
    print $ null ["abc"]
    print $ snoc [1, 2] 3
    print $ length [1, 2, 3]
    print $ length' [1, 2, 3]
    print (head [] :: Maybe ())
    print $ head ["abc", "123"]
    print (tail [] :: Maybe [()])
    print $ tail ["abc", "123"]
    print (last [] :: Maybe ())
    print $ last [1, 2, 3]
    print $ last ["a", "b", "c"]
    print (init [] :: Maybe [()])
    print $ init [1]
    print $ init [1, 2]
    print $ init [1, 2, 3]
    print $ uncons [1, 2, 3]
    print $ index [1] 4
    print $ index [1, 2, 3] 1
    print (index [] 0 :: Maybe ())
    print $ index [1, 2, 3] (-99)
    putStrLn "Find Index --"
    print $ findIndex (>= 2) [1, 2, 3]
    print $ findIndex (>= 99) [1, 2, 3]
    print $ findIndex (/= 10) []
    putStrLn "Find Last Index --"
    print $ findLastIndex (== 10) []
    print $ findLastIndex (== 10) [10, 5, 10, -1, 2, 10]
    print $ findLastIndex (== 10) [11, 12]
    putStrLn "Reverse --"
    print $ reverse [10, 20, 30]
    putStrLn "Concat --"
    print $ concat [[1, 2, 3], [4, 5], [6], []]
    print $ filter (< 4) [1, 2, 3, 4, 5, 6]
    print $ filter' (< 4) [1, 2, 3, 4, 5, 6]

flip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip f x y = f y x

const :: p1 -> p2 -> p1
const x _ = x

apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x

applyFlipped :: t1 -> (t1 -> t3) -> t3
applyFlipped = flip apply

singleton :: a -> [a]
singleton x = [x]

null :: [a] -> Bool
null [] = True
null _ = False

snoc :: [t] -> t -> [t]
snoc [] x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: Num p => [a] -> p
length [] = 0
length (_ : xs) = 1 + length xs

length' :: Num t => [a] -> t
length' l = go l 0
    where
        go [] acc = acc
        go (_ : xs') acc = go xs' (acc + 1)

head :: [a] -> Maybe a
head (x : _) = Just x
head _ = Nothing

tail :: [a] -> Maybe [a]
tail (_ : xs) = Just xs
tail _ = Nothing

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_ : xs) = last xs

init :: [a] -> Maybe [a]
init [] = Nothing
init l = Just $ go l
    where
        go [] = []
        go [_] = []
        go (x : xs) = x : go xs

data MyList a = MyList {
    hd :: a,
    tl :: [a]
} deriving Prelude.Show

uncons :: [a] -> Maybe (MyList a)
uncons [] = Nothing
uncons (x : xs) = Just $ MyList { hd = x, tl = xs }

index :: (Ord t, Num t) => [a] -> t -> Maybe a
index [] _ = Nothing
index _ x | x < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

findIndex :: Num a => (t -> Bool) -> [t] -> Maybe a
findIndex _ [] = Nothing
findIndex pred l = go 0 l
    where
        go acc (x : _) | pred x = Just acc
        go _ [] = Nothing
        go acc (_ : xs) = go (acc + 1) xs

findLastIndex :: Num a => (t -> Bool) -> [t] -> Maybe a
findLastIndex _ [] = Nothing 
findLastIndex pred l = go 0 l Nothing
    where
        go _ [] fi = fi
        go i (x : xs) fi = if pred x then go (i + 1) xs (Just i) else go (i + 1) xs fi 
        
reverse :: [a] -> [a]
reverse [] = []
reverse l = go l []
    where
        go [] acc = acc
        go (x : xs) acc = go xs (x : acc)

concat :: [[a]] -> [a]
concat [] = []
concat ([] : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x : xs) | pred x = x : filter pred xs
                     | otherwise = filter pred xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' pred = reverse <<< go []
    where
        go acc [] = acc
        go acc (x : xs) = if pred x then go (x : acc) xs else go acc xs