module Game(nextGeneration, createWorld) where

import Debug.Trace(trace)
import System.Random(randomR, StdGen)
import qualified Model as M

nextGeneration cellMatrix = map playGameOfLife cellMatrix
  where
    playGameOfLife cellArray = map maybeReproduce cellArray
    maybeReproduce aliveCell@(M.AliveCell(_)) = aliveCell
    maybeReproduce emptyCell@(M.EmptyCell(point)) =
      if (countNeighbours emptyCell isAlive cellMatrix) == 3
        then M.AliveCell(point)
        else emptyCell

countNeighbours cell cellState cellMatrix =
  length $ filter cellState $ neighbours (point cell) cellMatrix

neighbours (M.Point(y, x)) cellMatrix =
  filter (isNeighbour x y) $ flatten cellMatrix
  where
    flatten = foldl (++) []

isNeighbour x y cell =
  deltaX <= 1 && deltaY <= 1
  where
    deltaX = maxX - minX
    deltaY = maxY - minY
    maxX = max x x2
    maxY = max y y2
    minX = min x x2
    minY = min y y2
    (M.Point(y2, x2)) = point cell

point (M.EmptyCell(pt)) = pt
point (M.AliveCell(pt)) = pt

isAlive (M.AliveCell(_))      = True
isAlive (M.EmptyCell(_)) = False

createWorld rectangle@(M.Rectangle (width, height)) randomGen =
  M.World(rectangle, map createCellMatrix xs)
  where
    xs = [x | x <- [0..width]]
    createCellMatrix x = [createCell x y | y <- [0..height]]
    createCell x y =
      if createLiveCell x y
        then M.AliveCell(M.Point(x, y))
        else M.EmptyCell(M.Point(x, y))
    createLiveCell x y = x `mod` randNumber == 1
    (randNumber, _) = randomR (1,10) randomGen :: (Int, StdGen)
