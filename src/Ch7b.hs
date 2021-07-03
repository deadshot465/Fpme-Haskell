module Ch7b
  ( test
  ) where

import Prelude

import Data.Function ((&))
import Data.Text (pack, splitOn, unpack)

test :: IO ()
test = do
  print $ toCSV person
  print $ toCSV person == CSV "John, 25, Lawyer"
  print $ (toCSV person & fromCSV) == Just person
    where
      person = Person { name = FullName "John", age = Age 25, occupation = Lawyer }

newtype CSV = CSV [Char] deriving (Eq, Show)
newtype FullName = FullName [Char] deriving (Eq, Show)
newtype Age = Age Int deriving (Eq, Show)

data Occupation = Doctor | Dentist | Lawyer | Unemployed deriving (Eq, Show)

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  } deriving (Eq, Show)

class ToCSV a where
  toCSV :: a -> CSV

instance ToCSV Person where
  toCSV Person { name = name, age = age, occupation = occupation } = CSV $ show name ++ ", " ++ show age ++ ", " ++ show occupation

class ToCSV a => FromCSV a where
  fromCSV :: CSV -> Maybe a

instance FromCSV Person where
  fromCSV (CSV str) = case splitOn (pack ", ") (pack str) of
    [name, age, occupation] -> do
      age' <- Just (read $ unpack age :: Int)
      occupation' <- toOccupation $ unpack occupation
      pure $ Person { name = FullName $ unpack name
      , age = Age age'
      , occupation = occupation' }
    _ -> Nothing


toOccupation :: [Char] -> Maybe Occupation
toOccupation occ = case occ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing