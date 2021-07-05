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
  verifyMod4Semigroup
  verifyMod4Monoid

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

data Mod4 = Zero | One | Two | Three deriving (Eq, Show)

instance Semigroup Mod4 where
  (<>) Zero x = x
  (<>) x Zero = x
  (<>) One One = Two
  (<>) One Two = Three
  (<>) One Three = Zero
  (<>) Two One = Three
  (<>) Two Two = Zero
  (<>) Two Three = One
  (<>) Three One = Zero
  (<>) Three Two = One
  (<>) Three Three = Two

instance Monoid Mod4 where
  mempty = Zero

verifyMod4Semigroup :: IO ()
verifyMod4Semigroup = do
  putStrLn "Verifying Mod4 Semigroup Laws (1 test)"
  print $ (One <> Two) <> Three == One <> (Two <> Three)

verifyMod4Monoid :: IO ()
verifyMod4Monoid = do
  putStrLn "Verifying Mod4 Monoid Laws (1 test)"
  print $ One <> mempty == mempty <> One && One <> mempty == One

newtype First a = First (Maybe a) deriving (Eq, Show)
newtype Last a = Last (Maybe a) deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) (First Nothing) last = last
  (<>) first _ = first

instance Monoid (First a) where
  mempty = First Nothing

instance Semigroup (Last a) where
  (<>) first (Last Nothing) = first
  (<>) _ last = last

instance Monoid (Last a) where
  mempty = Last Nothing