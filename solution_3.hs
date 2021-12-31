{-# OPTIONS_GHC -Wall #-}

import qualified Data.List as List
import Data.Maybe (mapMaybe)

data Bin = O | I


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

countBinary :: Bin -> BinCount
countBinary O = BinCount 1 0
countBinary I = BinCount 0 1

parseBinary :: Char -> Maybe Bin
parseBinary '0' = Just O
parseBinary '1' = Just I
parseBinary _ = Nothing


binarySequence :: [Int]
binarySequence = fmap (2^) [0 :: Int ..]

boolToBin :: Bool -> Char
boolToBin True = '1'
boolToBin False = '0'

binToNum :: Char -> Int
binToNum '0' = 0
binToNum '1' = 1
binToNum _ = error "Not a binary number"

binToInt :: String -> Int
binToInt str = sum $ fmap (uncurry (*)) $ zip binarySequence $ fmap binToNum $ reverse str

main :: IO ()
main = do
  let tinput :: [[Bin]]
      tinput = List.transpose input

      count :: [BinCount]
      count = fmap (foldMap countBinary) tinput

      rate :: (Int -> Int -> Bool) -> String
      rate f = fmap boolToBin (fmap (\(BinCount x y) -> f x y) count)

      gammaRate :: String
      gammaRate = rate (>)

      epsilonRate :: String
      epsilonRate = rate (<)

      powerConsumption :: Int
      powerConsumption = binToInt gammaRate * binToInt epsilonRate

  print ("Gamma Rate: " ++ gammaRate)
  print ("Epsilon Rate: " ++ epsilonRate)
  print ("Power Consumption: " ++ show powerConsumption)
  where
    input = fmap (mapMaybe parseBinary) ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

