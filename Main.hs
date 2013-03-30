import System.Random(getStdGen)
import Control.Monad(when)
import qualified Model as M
import qualified Game as G

main = do
  gen <- getStdGen
  putStrLn "Press <enter> for next generation, q+<enter> to quit."
  waitForNextTick (world gen)
  where
    world gen = G.createWorld (M.Rectangle(16, 16)) gen

waitForNextTick world@(M.World (border, cellMatrix)) = do
  putStr $ "\n" ++ (show world)
  anyKey <- getLine
  when(not $ anyKey == "q") $ do
    waitForNextTick $ M.World(border, G.nextGeneration cellMatrix)
