import           Control.Monad
import           Data.List                      ( sort
                                                , sortOn
                                                , groupBy
                                                )
import           Data.Function                  ( on )

h :: [Int] -> (Int, Int)
h (x : y : []) = (x, y)

readTuple :: IO (Int, Int)
readTuple = do
  s <- getLine
  return $ h (map read $ words s)

solve :: Int -> [Int] -> [Int]
solve k ns =
  map fst
    $ sortOn snd
    $ map head
    $ filter ((<=) k . length)
    $ groupBy (on (==) fst)
    $ sort
    $ zip ns [0 ..]

solveTestCase :: IO ()
solveTestCase = do
  (n, k) <- readTuple
  s      <- getLine
  let ans = solve k $ map read $ words s
  putStrLn $ if null ans then "-1" else (unwords $ map show ans)

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t solveTestCase
