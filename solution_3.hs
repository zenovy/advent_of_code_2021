{-# OPTIONS_GHC -Wall #-}

import qualified Data.List as List
import Data.Maybe (mapMaybe)

data Bin = O | I

countBinary :: Bin -> (Int, Int) -> (Int, Int)
countBinary O (a,b) = (a + 1, b)
countBinary I (a,b) = (a, b + 1)

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
binToInt str = let binaryNum = fmap binToNum (reverse str)
               in sum (fmap (uncurry (*)) (zip binarySequence binaryNum))

main :: IO ()
main = do
  let tinput = List.transpose input
  let count = fmap (foldr countBinary (0,0)) tinput
  let rate f = fmap boolToBin (fmap (uncurry f) count)
  let gammaRate = rate (>)
  let epsilonRate = rate (<)
  let powerConsumption = (binToInt gammaRate) * (binToInt epsilonRate)
  print ("Gamma Rate: " ++ gammaRate)
  print ("Epsilon Rate: " ++ epsilonRate)
  print ("Power Consumption: " ++ show powerConsumption)
  where
    input = fmap (mapMaybe parseBinary) ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

