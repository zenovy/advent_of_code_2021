{-# OPTIONS_GHC -Wall #-}

getPrefix :: String -> String
getPrefix = takeWhile (/= ' ')

getValue :: String -> String -> Int
getValue command prefix = read (drop (1 + length prefix) command)

data Direction = Up | Down | Forward

parseDirection :: String -> Direction
parseDirection "up" = Up
parseDirection "down" = Down
parseDirection "forward" = Forward

data MoveCommand = MoveCommand
  { moveDirection :: Direction
  , moveCount :: Int
  }

parseMoveCommand :: String -> MoveCommand
parseMoveCommand command = MoveCommand (parseDirection prefix) value
  where
    prefix = getPrefix command
    value = getValue command prefix

move :: (Int, Int) -> MoveCommand -> (Int, Int)
move (x, y) (MoveCommand Up value) = (x, y - value)
move (x, y) (MoveCommand Down value) = (x, y + value)
move (x, y) (MoveCommand Forward value) = (x + value, y)

main :: IO ()
main = do
  let output = foldl move initPos $ fmap parseMoveCommand input
  print output
  where initPos = (0,0)
        input   = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
