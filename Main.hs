import System.Random
import qualified Model as M

createWorld rectangle@(M.Rectangle (width, height)) randomGen =
  M.World(rectangle, map createCellMatrix xs)
  where
    xs = [x | x <- [0..width]]
    createCellMatrix x = [createCell x y | y <- [0..height]]
    createCell x y = if createLiveCell x y then M.LiveCell(x, y) else M.EmptyCell
    createLiveCell x y = x `mod` randNumber == 1
    (randNumber, _) = randomR (1,10) randomGen :: (Int, StdGen)

main = do
  gen <- getStdGen
  putStr $ show (world gen)
  where
    world gen = createWorld (M.Rectangle(8, 8)) gen
