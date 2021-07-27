{-# LANGUAGE RankNTypes #-}
module Owoifier where

import Prelude

import Data.Text ( Text )
import Control.Monad ((>=>))

test :: IO ()
test = do
  putStrLn "Hello World"

class OwoifyError e where
  eof :: e

type OwoifyResult a = (Text, a)
type OwoifyFunction e a = OwoifyError e => Text -> Either e (OwoifyResult a)
data OError = EOF
newtype OwoifyParser e a = OwoifyParser (OwoifyFunction e a)

runParser :: OwoifyError e => OwoifyParser e a -> Text -> Either e (OwoifyResult a)
runParser (OwoifyParser mf) = mf

instance OwoifyError OError where
  eof = EOF

instance OwoifyError e => Functor (OwoifyParser e) where
  fmap f (OwoifyParser mg) = OwoifyParser $ fmap (fmap f) . mg

instance OwoifyError e => Applicative (OwoifyParser e) where
  (<*>) (OwoifyParser mf) (OwoifyParser mg) = OwoifyParser $ \s -> do
    (t, f) <- mf s
    (t', a) <- mg t
    pure (t <> t', f a)
  pure x = OwoifyParser $ \s -> pure (s, x)

instance OwoifyError e => Monad (OwoifyParser e) where
  (>>=) (OwoifyParser mf) g = OwoifyParser $ mf >=> \(t, a) -> runParser (g a) t

