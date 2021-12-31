{-# OPTIONS_GHC -Wall #-}

import qualified Data.List as List
import Data.Maybe (mapMaybe)

data BinCount = BinCount
  { bc0 :: Int
  , bc1 :: Int
  }
  deriving Show

-- A semigroup is any type which can be smushed into itself. Here we define
-- smushing via pointwise addition.
instance Semigroup BinCount where
  BinCount x1 y1 <> BinCount x2 y2 = BinCount (x1 + x2) (y1 + y2)

-- A monoid is any semigroup which has a zero that doesn't change things when
-- you smush it in.
instance Monoid BinCount where
  mempty = BinCount 0 0

countBinary :: Bool -> BinCount
countBinary False = BinCount 1 0
countBinary True = BinCount 0 1

parseBinary :: Char -> Maybe Bool
parseBinary '0' = Just False
parseBinary '1' = Just True
parseBinary _ = Nothing


binarySequence :: [Int]
binarySequence = iterate (2 *) 1

boolToBin :: Bool -> String
boolToBin = show . boolToNum

boolToNum :: Bool -> Int
boolToNum False = 0
boolToNum True = 1

binToInt :: [Bool] -> Int
binToInt
  = sum
  . fmap (uncurry (*))
  . zip binarySequence
  . fmap boolToNum
  . reverse

main :: IO ()
main = do
  let rate :: (Int -> Int -> Bool) -> [Bool]
      rate f
        = fmap (\(BinCount x y) -> f x y)
        $ fmap (foldMap countBinary)
        $ List.transpose input

      gammaRate :: [Bool]
      gammaRate = rate (>)

      epsilonRate :: [Bool]
      epsilonRate = rate (<)

      powerConsumption :: Int
      powerConsumption = binToInt gammaRate * binToInt epsilonRate

  print ("Gamma Rate: " ++ concatMap boolToBin gammaRate)
  print ("Epsilon Rate: " ++ concatMap boolToBin epsilonRate)
  print ("Power Consumption: " ++ show powerConsumption)
  where
    input = fmap (mapMaybe parseBinary) ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

