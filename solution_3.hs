import qualified Data.List as List

countBinary :: (Int, Int) -> Char -> (Int, Int)
countBinary (a,b) '0' = (a + 1, b)
countBinary (a,b) '1' = (a, b + 1)
countBinary _ x = error ("Non-binary character found: " ++ show x)

binarySequence :: [Int]
binarySequence = fmap (2^) [0..]

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

main = do
  let tinput = List.transpose input
  let count = fmap (foldl countBinary (0,0)) tinput
  let rate f = fmap boolToBin (fmap (uncurry f) count)
  let gammaRate = rate (>)
  let epsilonRate = rate (<)
  let powerConsumption = (binToInt gammaRate) * (binToInt epsilonRate)
  print ("Gamma Rate: " ++ gammaRate)
  print ("Epsilon Rate: " ++ epsilonRate)
  print ("Power Consumption: " ++ show powerConsumption)
  where
    input = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

