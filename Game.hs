module Game(nextGeneration, createWorld) where

import Debug.Trace(trace)
import System.Random(randomR, StdGen)
import qualified Model as M

nextGeneration cellMatrix = map playGameOfLife cellMatrix
  where
    playGameOfLife cellArray = map maybeReproduce cellArray
    maybeReproduce life@(M.Life(_)) = life
    maybeReproduce emptiness@(M.Emptiness(point)) =
      if (countNeighbours emptiness isAlive cellMatrix) == 3
        then M.Life(point)
        else emptiness

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

point (M.Emptiness(pt)) = pt
point (M.Life(pt)) = pt

isAlive (M.Life(_))      = True
isAlive (M.Emptiness(_)) = False

createWorld rectangle@(M.Rectangle (width, height)) randomGen =
  M.World(rectangle, map createCellMatrix xs)
  where
    xs = [x | x <- [0..width]]
    createCellMatrix x = [createCell x y | y <- [0..height]]
    createCell x y =
      if createLiveCell x y
        then M.Life(M.Point(x, y))
        else M.Emptiness(M.Point(x, y))
    createLiveCell x y = x `mod` randNumber == 1
    (randNumber, _) = randomR (1,10) randomGen :: (Int, StdGen)
