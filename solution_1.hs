collect :: [Int] -> Int
collect []       = 0
collect (x:[])   = 0
collect (x:y:xs) = (if (x < y) then 1 else 0) + collect (y:xs)

main = do
  let output = collect input where input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
  print output
