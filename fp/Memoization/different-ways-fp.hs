import qualified Data.Map.Strict               as M
import           Data.Maybe

main :: IO ()
main =
  interact
    $ unlines
    . map show
    . solve
    . map (makeTuple . map read . words)
    . tail
    . lines

makeTuple :: [Int] -> (Int, Int)
makeTuple [x, y] = (x, y)

p = 100000007

solve :: [(Int, Int)] -> [Int]
solve = fst . foldr f ([], M.empty)
 where
  f
    :: (Int, Int)
    -> ([Int], M.Map (Int, Int) Int)
    -> ([Int], M.Map (Int, Int) Int)
  f (n, k) (xs, m) = let (x, m') = runSolver m n k in (x : xs, m')

counter :: M.Map (Int, Int) Int -> Int -> Int -> M.Map (Int, Int) Int
counter m n 0 = M.insert (n, 0) 1 m
counter m n k | k == n    = M.insert (k, n) 1 m
              | otherwise = M.insert (n, k) ((x + y) `mod` p) m''
 where
  (x, m' ) = runSolver m (n - 1) (k - 1)

  (y, m'') = runSolver m' (n - 1) k

runSolver :: M.Map (Int, Int) Int -> Int -> Int -> (Int, M.Map (Int, Int) Int)
runSolver m n k = case M.lookup (n, k) m of
  Nothing -> let m' = counter m n k in (m' M.! (n, k), m')
  Just x  -> (x, m)