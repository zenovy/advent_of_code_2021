{-# OPTIONS_GHC -Wall #-}

import Data.Maybe (mapMaybe)

getPrefix :: String -> String
getPrefix = takeWhile (/= ' ')

getValue :: String -> String -> Int
getValue command prefix = read (drop (1 + length prefix) command)

data Direction = Up | Down | Forward

parseDirection :: String -> Maybe Direction
parseDirection "up" = Just Up
parseDirection "down" = Just Down
parseDirection "forward" = Just Forward
parseDirection _ = Nothing

data MoveCommand = MoveCommand
  { moveDirection :: Direction
  , moveCount :: Int
  }

parseMoveCommand :: String -> Maybe MoveCommand
parseMoveCommand command = do
  -- If we fail to parse the direction, we will fail to parse the entire move
  -- command
  direction <- parseDirection prefix
  pure $ MoveCommand direction value
  where
    prefix = getPrefix command
    value = getValue command prefix

move :: (Int, Int) -> MoveCommand -> (Int, Int)
move (x, y) (MoveCommand Up value) = (x, y - value)
move (x, y) (MoveCommand Down value) = (x, y + value)
move (x, y) (MoveCommand Forward value) = (x + value, y)

main :: IO ()
main = do
  let output = foldr (flip move) initPos
               -- mapMaybe will throw away any Nothings it comes across
             $ mapMaybe parseMoveCommand input
  print output
  where initPos = (0,0)
        input   = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
