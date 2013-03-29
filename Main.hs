import qualified Model as M

createWorld rectangle@(M.Rectangle (width, height)) =
  M.World(rectangle, map createCellMatrix xs)
  where
    xs = [x | x <- [0..width]]
    createCellMatrix x = [createCell x y | y <- [0..height]]
    createCell x y = if even y then M.EmptyCell else M.LiveCell(x, y)

main = do
  putStrLn $ show world
  where
    world = createWorld $ M.Rectangle(8, 8)
