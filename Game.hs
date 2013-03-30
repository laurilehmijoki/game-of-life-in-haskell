module Game(nextGeneration, createWorld) where

import Debug.Trace(trace)
import System.Random(randomR, StdGen)
import Data.Maybe
import qualified Model as M
import qualified Rules as R

nextGeneration cellMatrix = map playGameOfLife cellMatrix
  where
    playGameOfLife cellArray = map applyRules cellArray
    applyRules cell =
      let maybeChange = newCellValue cell cellMatrix
      in maybe cell id maybeChange

newCellValue cell cellMatrix = applyRules
  where
    applyRule cell rule = rule cellMatrix cell
    applyRules =
      let changes = filter isJust $ map (applyRule cell) R.rules
      in if length changes > 1
           then error "Only one rule may apply to a cell"
           else if null changes then Nothing else head changes

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
