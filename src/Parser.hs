{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Prelude

import Control.Applicative (empty)
import Control.Monad (ap, (>=>))
import Data.Function ((&))
import Data.Text (Text(..), uncons, pack)

test :: IO ()
test = do
  print $ parse' char (pack "ABC")
  print $ parse' twoChars (pack "ABC")
  print $ parse' threeChars (pack "ABC")
  print $ parse' threeChars (pack "A")
  print $ parse' (count 3 char) (pack "ABCD")

type ParserState a = (Text, a)

data PError = EOF deriving (Eq, Show)

class ParserError e where
  eof :: e

instance ParserError PError where
  eof = EOF

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
  (<*>) = ap

parse :: Parser e a -> ParserFunction e a
parse (Parser f) = f

parse' :: Parser PError a -> ParserFunction PError a
parse' = parse

char :: Parser e Char
char = Parser (\s -> case uncons s of
  Nothing -> Left eof
  Just (c, s) -> Right (s, c))

twoChars :: Parser e (Char, Char)
twoChars = (,) <$> char <*> char

threeChars :: Parser e [Char]
threeChars = (\c1 c2 c3 -> [c1, c2, c3]) <$> char <*> char <*> char


count :: Applicative f => Int -> f a -> f [a]
count n p | n <= 0 = pure empty
          | otherwise = sequenceA (replicate n p)

instance Monad (Parser e) where
  -- (>>=) mf mx = Parser (parse mf >=> \(s1, x) -> parse (mx x) s1)
  (>>=) mf mx = Parser (\s -> do
    (s1, x) <- parse mf s
    parse (mx x) s1)