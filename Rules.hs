module Rules(rules) where

import Debug.Trace(trace)
import qualified Model as M

rules = [reproductionRule, overcrowdingRule, underpopulationRule]

reproductionRule cellMatrix (M.AliveCell(_)) = Nothing
reproductionRule cellMatrix emptyCell@(M.EmptyCell(point)) =
  if (countNeighbours emptyCell isAlive cellMatrix) == 3
    then Just $ M.AliveCell point -- a new cell
    else Nothing

overcrowdingRule cellMatrix (M.EmptyCell(_)) = Nothing
overcrowdingRule cellMatrix aliveCell@(M.AliveCell(point)) =
  if (countNeighbours aliveCell isAlive cellMatrix) > 3
    then Just $ M.EmptyCell point -- the cell dies
    else Nothing

underpopulationRule cellMatrix (M.EmptyCell(_)) = Nothing
underpopulationRule cellMatrix aliveCell@(M.AliveCell(point)) =
  if (countNeighbours aliveCell isAlive cellMatrix) < 2
    then Just $ M.EmptyCell point -- the cell dies
    else Nothing

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

isAlive (M.AliveCell(_)) = True
isAlive (M.EmptyCell(_)) = False
