import           Control.Monad

readList_ :: IO [Int]
readList_ = do
  n <- readLn :: IO Int
  map read . words <$> getLine

printAns :: Bool -> IO ()
printAns x = putStrLn $ if x then "YES" else "NO"

isValidBST :: [Int] -> Bool
isValidBST []       = True
isValidBST (x : xs) = case span (< x) xs of
  ([]           , []            ) -> True
  (left@(l : ls), []            ) -> isValidBST left
  ([]           , right@(r : rs)) -> all (> x) rs && isValidBST right
  (left@(l : ls), right@(r : rs)) ->
    isValidBST left && all (> x) rs && isValidBST right


main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ isValidBST <$> readList_ >>= printAns