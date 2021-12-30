{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall     #-}

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

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
parseMoveCommand command =
  let (unparsed_direction, drop 1 -> unparsed_value) = break (== ' ') command
   in MoveCommand <$> parseDirection unparsed_direction
                  <*> readMaybe unparsed_value

data Pos = Pos
  { posX :: Int
  , posY :: Int
  }
  deriving Show

move :: MoveCommand -> Pos -> Pos
move (MoveCommand Up value) (Pos x y) = Pos x (y - value)
move (MoveCommand Down value) (Pos x y) = Pos x (y + value)
move (MoveCommand Forward value) (Pos x y) = Pos (x + value) y

main :: IO ()
main = do
  let output = foldr move initPos
               -- mapMaybe will throw away any Nothings it comes across
             $ mapMaybe parseMoveCommand input
  print output
  where initPos = Pos 0 0
        input   = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
