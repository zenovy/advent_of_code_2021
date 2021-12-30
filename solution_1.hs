{-# OPTIONS_GHC -Wall #-}

collect :: [Int] -> Int
collect xs = sum $ zipWith (\x y -> if (x < y) then 1 else 0) xs $ drop 1 xs


main :: IO ()
main = do
  let output = collect input where input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
  print output
