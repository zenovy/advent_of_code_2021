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

-- A semigroup is any type which can be smushed into itself. Here we define
-- smushing via pointwise addition.
instance Semigroup Pos where
  Pos x1 y1 <> Pos x2 y2 = Pos (x1 + x2) (y1 + y2)

-- A monoid is any semigroup which has a zero that doesn't change things when
-- you smush it in.
instance Monoid Pos where
  mempty = Pos 0 0


move :: MoveCommand -> Pos
move (MoveCommand Up value) = Pos 0 (-value)
move (MoveCommand Down value) = Pos 0 value
move (MoveCommand Forward value) = Pos value 0

main :: IO ()
main = do
  let output = foldMap move
               -- mapMaybe will throw away any Nothings it comes across
             $ mapMaybe parseMoveCommand input
  print output
  where input   = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
