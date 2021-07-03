module Ch5
    ( test
    ) where

import Prelude hiding (flip, const, null, length, head, tail, last, init, uncons, reverse, concat, filter, take, drop, takeWhile, dropWhile, zip, unzip)

import Control.Arrow ((<<<), (>>>))
import Data.Function ((&))

test :: IO ()
test = do
    print (flip const 1 2)
    putStrLn "Null --"
    print $ null []
    print $ null ["abc"]
    putStrLn "Snoc --"
    print $ snoc [1, 2] 3
    putStrLn "Length --"
    print $ length [1, 2, 3]
    print $ length' [1, 2, 3]
    putStrLn "Head --"
    print (head [] :: Maybe ())
    print $ head ["abc", "123"]
    putStrLn "Tail --"
    print (tail [] :: Maybe [()])
    print $ tail ["abc", "123"]
    putStrLn "Last --"
    print (last [] :: Maybe ())
    print $ last [1, 2, 3]
    print $ last ["a", "b", "c"]
    putStrLn "Init --"
    print (init [] :: Maybe [()])
    print $ init [1]
    print $ init [1, 2]
    print $ init [1, 2, 3]
    putStrLn "Uncons --"
    print $ uncons [1, 2, 3]
    putStrLn "Index --"
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
    putStrLn "Filter --"
    print $ filter (< 4) [1, 2, 3, 4, 5, 6]
    print $ filter' (< 4) [1, 2, 3, 4, 5, 6]
    putStrLn "CatMaybes --"
    print $ catMaybes [Just 1, Nothing, Just 2, Nothing, Nothing, Just 5]
    putStrLn "Range --"
    print $ range 1 10
    print $ range 3 (-3)
    print $ range' 1 10
    print $ range' 3 (-3)
    putStrLn "Take --"
    print $ take 5 [12, 13, 14]
    print $ take 5 [-7, 9, 0, 12, -13, 45, 976, -19]
    putStrLn "Drop --"
    print $ drop 2 [1, 2, 3, 4, 5, 6, 7]
    print $ drop 10 ([] :: [()])
    putStrLn "TakeWhile --"
    print $ takeWhile (> 3) [5, 4, 3, 99, 101]
    print $ takeWhile (== -17) [1, 2, 3]
    putStrLn "DropWhile --"
    print $ dropWhile (> 3) [5, 4, 3, 99, 101]
    print $ dropWhile (== -17) [1, 2, 3]
    putStrLn "TakeEnd --"
    print $ takeEnd 3 [1, 2, 3, 4, 5, 6]
    print $ takeEnd 10 [1]
    putStrLn "DropEnd --"
    print $ dropEnd 3 [1, 2, 3, 4, 5, 6]
    print $ dropEnd 10 [1]
    putStrLn "Zip --"
    print $ zip [1, 2, 3] ["a", "b", "c", "d", "e"]
    print $ zip ["a", "b", "c", "d", "e"] [1, 2, 3]
    print $ zip ([] :: [()]) [1, 2]
    putStrLn "Unzip --"
    print $ unzip [(1, "a"), (2, "b"), (3, "c")]
    print $ unzip [("a", 1), ("b", 2), ("c", 3)]
    print $ unzip ([] :: [((), ())])

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

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x) : xs) = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs

range :: (Ord a, Num a) => a -> a -> [a]
range start end | end > start = start : range (start + 1) end
                | end < start = start : range (start - 1) end
                | otherwise = singleton end

range' :: (Ord a, Num a) => a -> a -> [a]
range' start end = go [] end start
    where
        go acc start' end' | start' == end' = end' : acc
                           | otherwise = go (start' : acc) (start' + step) end'
        step = if end > start then (-1) else 1

take :: (Num t, Ord t) => t -> [a] -> [a]
take n = reverse . go [] (max 0 n)
    where
        go acc 0 _ = acc
        go acc _ [] = acc
        go acc n' (x : xs) = go (x : acc) (n' - 1) xs

drop :: (Eq t, Num t) => t -> [a] -> [a]
drop 0 l = l
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred = reverse . go [] pred
    where
        go acc _ [] = acc
        go acc pred' (x : xs) = if pred' x then go (x : acc) pred' xs else acc

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeEnd :: (Num a1, Ord a1) => a1 -> [a2] -> [a2]
takeEnd n = go >>> snd
    where
        go [] = (0, [])
        go (x : xs) = go xs & \(c, nl) -> (c + 1, if c < n then x : nl else nl)

dropEnd :: (Num a1, Ord a1) => a1 -> [a2] -> [a2]
dropEnd n = go >>> snd
    where
        go [] = (0, [])
        go (x : xs) = go xs & \(c, nl) -> (c + 1, if c < n then nl else x : nl)

zip :: [a] -> [b] -> [(a, b)]
zip l = reverse <<< go [] l
    where
        go acc [] _ = acc
        go acc _ [] = acc
        go acc (x : xs) (y : ys) = go ((x, y) : acc) xs ys

unzip :: [(a1, a2)] -> ([a1], [a2])
unzip [] = ([], [])
unzip ((x, y) : ts) = unzip ts & \(xs, ys) -> (x : xs, y : ys)