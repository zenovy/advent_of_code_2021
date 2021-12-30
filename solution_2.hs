getPrefix :: String -> String
getPrefix []       = []
getPrefix (' ':xs) = []
getPrefix (x:xs)   = x: getPrefix xs

getValue :: String -> String -> Int
getValue command prefix = read (drop (1 + length prefix) command)

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) command
  | prefix == "up"      = (x, y - value)
  | prefix == "down"    = (x, y + value)
  | prefix == "forward" = (x + value, y)
  | otherwise           = error ("Command not defined in string '" ++ command ++ "'")
  where prefix = getPrefix command
        value = getValue command prefix

main = do
  let output = foldl move initPos input
  print output
  where initPos = (0,0)
        input   = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
