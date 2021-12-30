getPrefix :: String -> String
getPrefix []       = []
getPrefix (' ':xs) = []
getPrefix (x:xs)   = x: getPrefix xs

getValue :: String -> String -> Int
getValue command prefix = read (drop (1 + length prefix) command)

data MoveCommand = MoveCommand
  { moveDirection :: String
  , moveCount :: Int
  }

parseMoveCommand :: String -> MoveCommand
parseMoveCommand command = MoveCommand prefix value
  where
    prefix = getPrefix command
    value = getValue command prefix

move :: (Int, Int) -> MoveCommand -> (Int, Int)
move (x, y) (MoveCommand prefix value)
  | prefix == "up"      = (x, y - value)
  | prefix == "down"    = (x, y + value)
  | prefix == "forward" = (x + value, y)
  | otherwise           = error ("Command not defined in string '" ++ prefix ++ "'")

main = do
  let output = foldl move initPos $ fmap parseMoveCommand input
  print output
  where initPos = (0,0)
        input   = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
