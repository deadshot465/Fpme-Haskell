{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Prelude hiding (fail)

import Control.Applicative (empty, Alternative)
import Control.Arrow ((>>>))
import Control.Monad (ap, (>=>), void)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Text (Text(..), uncons, pack)
import GHC.Base ((<|>))
import GHC.Unicode (isAlpha, isDigit)

test :: IO ()
test = do
  print $ parse' char (pack "ABC")
  print $ parse' twoCharsB (pack "ABC")
  print $ parse' threeCharsB (pack "ABC")
  print $ parse' threeCharsB (pack "A")
  print $ parse' (count 3 char) (pack "ABCD")
  print $ parse' (count 3 digit) $ pack "123456"
  print $ parse' (count 3 digit) $ pack "abc456"
  print $ parse' (count 4 letter) $ pack "Freddy"
  print $ parse' (count 10 alphaNum) $ pack "a1b2c3d4e5"
  print $ parse' (count 10 alphaNum) $ pack "######"
  print $ parse' (atMost (-2) alphaNum) $ pack "a1b2c3"
  print $ parse' (atMost 2 alphaNum) $ pack "$_$"
  print $ parse' (atMost 2 alphaNum) $ pack "a1b2c3"
  print $ parse' monthFirst $ pack "12/31/1999"
  print $ parse' (some' digit) $ pack "2343423423abc"
  print $ parse' (many' digit) $ pack "_2343423423abc"
  print $ parse' (some' digit) $ pack "_2343423423abc"
  print $ parse' ugly $ pack "17, some words"
  print $ parse' ugly $ pack "5432, some more words1234567890"

type ParserState a = (Text, a)

data PError = EOF | InvalidChar Text | InvalidDate Text deriving (Eq, Show)

class ParserError e where
  eof :: e
  invalidChar :: Text -> e
  invalidDate :: Text -> e

instance ParserError PError where
  eof = EOF
  invalidChar msg = InvalidChar msg
  invalidDate msg = InvalidDate msg

type ParserFunction e a = ParserError e => Text -> Either e (ParserState a)
newtype Parser e a = Parser (ParserFunction e a)

instance Functor (Parser e) where
  fmap f p = Parser (fmap (fmap f) . parse p)

instance Applicative (Parser e) where
  pure x = Parser (\s -> pure (s, x))
  {- (<*>) p1 p2 = Parser (\s -> case parse p1 s of
    Left err -> Left err
    Right (s1, h) -> case parse p2 s1 of
      Left err -> Left err
      Right (s2, x) -> Right (s2, h x)) -}
  -- (<*>) = ap
  (<*>) p1 p2 = Parser (\s -> do
    (s1, h) <- parse p1 s
    (s2, x) <- parse p2 s1
    pure (s2, h x))

parse :: Parser e a -> ParserFunction e a
parse (Parser f) = f

parse' :: Parser PError a -> ParserFunction PError a
parse' = parse

char :: Parser e Char
char = Parser (\s -> case uncons s of
  Nothing -> Left eof
  Just (c, s) -> Right (s, c))

twoCharsA :: Parser e (Char, Char)
twoCharsA = (,) <$> char <*> char

twoCharsB :: Parser e (Char, Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure (c1, c2)

threeCharsA :: Parser e [Char]
threeCharsA = (\c1 c2 c3 -> [c1, c2, c3]) <$> char <*> char <*> char

threeCharsB :: Parser e [Char]
threeCharsB = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure [c1, c2, c3]

count :: Applicative f => Int -> f a -> f [a]
count n p | n <= 0 = pure empty
          | otherwise = sequenceA (replicate n p)

instance Monad (Parser e) where
  (>>=) mf mx = Parser (parse mf >=> \(s1, x) -> parse (mx x) s1)
  {- (>>=) mf mx = Parser (\s -> do
    (s1, x) <- parse mf s
    parse (mx x) s1) -}

satisfy :: ParserError e => String -> (Char -> Bool) -> Parser e Char
satisfy msg pred = char >>= \c -> if pred c then pure c else fail $ invalidChar (pack msg)

fail :: ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

instance Alternative (Parser e) where
  (<|>) p1 p2 = Parser (\s -> case parse p1 s of
    Left _ -> parse p2 s
    Right x -> Right x)

digit :: ParserError e => Parser e Char
digit = satisfy "digit" isDigit

letter :: ParserError e => Parser e Char
letter = satisfy "letter" isAlpha

alphaNum :: ParserError e => Parser e Char
alphaNum = digit <|> letter <|> fail (invalidChar $ pack "alphaNum")

newtype Year = Year Int deriving (Show)
newtype Month = Month Int deriving (Show)
newtype Day = Day Int deriving (Show)

data DateFormat = YearFirst | MonthFirst deriving (Show)

data DateParts = DateParts
  { year :: Year
  , month :: Month
  , day :: Day
  , format :: DateFormat } deriving (Show)

{- atMost :: Int -> Parser e a -> Parser e [a]
atMost n p | n <= 0 = pure []
           | otherwise = optional [] $ p >>= \c -> (c :) <$> atMost (n - 1) p -}

atMost :: Int -> Parser e a -> Parser e [a]
atMost n p | n <= 0 = pure []
           | otherwise = optional [] $ p >>= \c -> (c :) <$> atMost (n - 1) p

optional :: Alternative f => a -> f a -> f a
optional x p = p <|> pure x

range :: Int -> Int -> Parser e a -> Parser e [a]
range min max p | min < 0 || max <= 0 || max < min = pure []
                | otherwise = count min p >>= \cs -> (cs <>) <$> atMost (max - min) p

constChar :: ParserError e => Char -> Parser e ()
constChar = void . constChar'

constChar' :: ParserError e => Char -> Parser e Char
constChar' c = satisfy (show c) (== c)

digitsToNum :: String -> Int
digitsToNum s = read s :: Int

yearFirst :: ParserError e => Parser e DateParts
yearFirst = do
  year <- count 4 digit <&> (Year . digitsToNum)
  constChar '-'
  month <- range 1 2 digit <&> (Month . digitsToNum)
  constChar '-'
  day <- range 1 2 digit <&> (Day . digitsToNum)
  pure $ DateParts { year = year, month = month, day = day, format = YearFirst }

monthFirst :: ParserError e => Parser e DateParts
monthFirst = do
  month <- Month . digitsToNum <$> range 1 2 digit
  constChar '/'
  day <- Day . digitsToNum <$> range 1 2 digit
  constChar '/'
  year <- Year . digitsToNum <$> count 4 digit
  pure $ DateParts { year = year, month = month, day = day, format = MonthFirst }

date :: ParserError e => Parser e DateParts
date = monthFirst <|> yearFirst <|> fail (invalidDate $ pack "Invalid date.")

some :: Alternative f => t -> f a -> f (NonEmpty a)
some cons p = (:|) <$> p <*> many cons p

some' :: Alternative f => f a -> f [a]
some' p = toList <$> some (:) p

many :: Alternative f => t -> f a -> f [a]
many cons p = toList <$> some cons p <|> pure []

many' :: Alternative f => f a -> f [a]
many' = many (:)

ugly :: ParserError e => Parser e [[Char]]
ugly = do
  p1 <- range 1 4 digit
  constChar ','
  constChar ' '
  p2 <- some' (letter <|> constChar' ' ')
  p3 <- many' digit
  pure [p1, p2, p3]