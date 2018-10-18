import           Control.Monad
import qualified Data.Map.Strict               as Map

getMap :: [(Int, Int)] -> Map.Map Int [Int]
getMap = foldr (\(k, a) m -> Map.insertWith (++) k [a] m) Map.empty

-- [1,1,2] => False
-- [1,2,3] => True
-- [] => True

equalValues :: [Int] -> Bool
equalValues []       = True
equalValues (x : xs) = all (== x) xs

validate :: [(Int, Int)] -> Bool
validate xs = all equalValues (getMap xs)


h :: [Int] -> (Int, Int)
h xs = (head xs, xs !! 1)


readTuple :: IO (Int, Int)
readTuple = do
  s <- getLine
  return $ h (map read $ words s)

readListOfTuples :: IO ()
readListOfTuples = do
  n  <- readLn :: IO Int
  xs <- replicateM n readTuple
  putStrLn (if validate xs then "YES" else "NO")


main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t readListOfTuples

