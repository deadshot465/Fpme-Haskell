{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Prelude hiding (fail)

import Control.Applicative (empty, Alternative)
import Control.Monad (ap, (>=>))
import Data.Function ((&))
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

type ParserState a = (Text, a)

data PError = EOF | InvalidChar Text deriving (Eq, Show)

class ParserError e where
  eof :: e
  invalidChar :: Text -> e

instance ParserError PError where
  eof = EOF
  invalidChar msg = InvalidChar msg

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
  -- (>>=) mf mx = Parser (parse mf >=> \(s1, x) -> parse (mx x) s1)
  (>>=) mf mx = Parser (\s -> do
    (s1, x) <- parse mf s
    parse (mx x) s1)

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