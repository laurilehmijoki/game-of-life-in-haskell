module Model where

data Borders = Rectangle(Int, Int) | NoBorder deriving Show

data Point = Point(Int, Int) -- y, x
instance Show Point where
  show (Point(y, x)) = "(" ++ show x ++ "," ++ show y ++ ")"

data Cell = AliveCell(Point) | EmptyCell(Point)
instance Show Cell where
  show (EmptyCell(Point(y, x))) = " "
  show (AliveCell(_))      = "O"

data World = World(Borders, [[Cell]])
instance Show World where
  show (World (_, cellMatrix)) =
    unlines $ map show cellMatrix
