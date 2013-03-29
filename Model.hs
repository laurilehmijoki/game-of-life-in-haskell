module Model where

data Borders = Rectangle(Int, Int) | NoBorder deriving Show

data Cell = LiveCell(Int, Int) | EmptyCell
instance Show Cell where
  show EmptyCell = "_"
  show _         = "c"

data World = World(Borders, [[Cell]])
instance Show World where
  show (World (_, cellMatrix)) =
    unlines $ map (\cellArray -> show cellArray) cellMatrix
