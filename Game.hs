module Game(nextGeneration, createWorld) where

import System.Random(randomR, StdGen)
import qualified Model as M
import qualified Rules as R

nextGeneration cellMatrix = map playGameOfLife cellMatrix
  where
    playGameOfLife cellArray = map (R.reproductionRule cellMatrix) cellArray

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
