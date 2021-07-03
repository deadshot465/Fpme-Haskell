module Ch9 where

import Prelude hiding (Semigroup(..), Monoid(..))

test :: IO ()
test = do
  print $ ATrue <> ATrue
  print $ ATrue <> AFalse
  print $ AFalse <> AFalse
  print $ mempty <> ATrue == ATrue
  print $ mempty <> AFalse == ATrue
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  verifyOrBoolSemigroup
  verifyOrBoolMonoid

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

data AndBool = ATrue | AFalse deriving (Eq, Show)

instance Semigroup AndBool where
  (<>) ATrue ATrue = ATrue
  (<>) ATrue AFalse = AFalse
  (<>) AFalse ATrue = AFalse
  (<>) AFalse AFalse = AFalse

instance Monoid AndBool where
  mempty = ATrue

verifyAndBoolSemigroup :: IO ()
verifyAndBoolSemigroup = do
  putStrLn "Verifying AndBool Semigroup Laws (1 test)"
  print $ (ATrue <> AFalse) <> ATrue == ATrue <> (AFalse <> ATrue)

verifyAndBoolMonoid :: IO ()
verifyAndBoolMonoid = do
  putStrLn "Verifying AndBool Monoid Laws (2 tests)"
  print $ (ATrue <> mempty) == (mempty <> ATrue) && mempty <> ATrue == ATrue
  print $ (AFalse <> mempty) == (mempty <> AFalse) && mempty <> AFalse == AFalse

data OrBool = OTrue | OFalse deriving (Eq, Show)

instance Semigroup OrBool where
  (<>) OFalse OFalse = OFalse
  (<>) _ _ = OTrue

instance Monoid OrBool where
  mempty = OFalse

verifyOrBoolSemigroup :: IO ()
verifyOrBoolSemigroup = do
  putStrLn "Verifying OrBool Semigroup Laws (1 test)"
  print $ (OTrue <> OFalse) <> OTrue == OTrue <> (OFalse <> OTrue)

verifyOrBoolMonoid :: IO ()
verifyOrBoolMonoid = do
  putStrLn "Verifying OrBool Monoid Laws (2 tests)"
  print $ (OTrue <> mempty) == (mempty <> OTrue) && mempty <> OTrue == OTrue
  print $ (OFalse <> mempty) == (mempty <> OFalse) && mempty <> OFalse == OFalse