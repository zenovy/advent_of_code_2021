move :: (Int, Int) -> String -> (Int, Int)
move (x, y) command
  | isPrefix up command      = (x, y - read (drop (length up) command))
  | isPrefix down command    = (x, y + read (drop (length down) command))
  | isPrefix forward command = (x + read (drop (length forward) command), y)
  | otherwise                   = error ("Command not defined in string '" ++ command ++ "'")
  where up = "up "
        down = "down "
        forward = "forward "
        isPrefix :: String -> String -> Bool
        isPrefix a b = take (length a) b == a

main = do
  let output = foldl move initPos input 
  print output
  where initPos = (0,0)
        input   = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
