{-# LANGUAGE RankNTypes #-}
module Parser where

import Prelude

import Data.Text (Text(..), uncons)

test :: IO ()
test = do
  print "Hello"

type ParserState a = (Text, a)

data PError = EOF deriving (Eq, Show)

class ParserError e where
  eof :: e

type ParserFunction e a = ParserError e => Text -> Either e (ParserState a)
newtype Parser e a = Parser (ParserFunction e a)

instance Functor (Parser e) where
  fmap f p = Parser (fmap (fmap f) . parse p)

instance Applicative (Parser e) where
  pure x = Parser (\s -> pure (s, x))
  (<*>) p1 p2 = Parser (\s -> case parse p1 s of
    Left err -> Left err
    Right (s1, h) -> case parse p2 s1 of
      Left err -> Left err
      Right (s2, x) -> Right (s2, h x))

parse :: Parser e a -> ParserFunction e a
parse (Parser f) = f

char :: Parser e Char
char = Parser (\s -> case uncons s of
  Nothing -> Left eof
  Just (c, s) -> Right (s, c))