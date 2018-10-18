import           Control.Monad
import           Data.List                      ( sort
                                                , sortOn
                                                , groupBy
                                                )
import           Text.Printf

type Point = (Int, Int)

h :: [Int] -> (Int, Int)
h (x : y : []) = (x, y)

readTuple :: IO (Int, Int)
readTuple = do
  s <- getLine
  return $ h (map read $ words s)

euclideanDist :: Point -> Point -> Double
euclideanDist (x1, y1) (x2, y2) =
  sqrt . fromIntegral $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2


perimeter :: [Point] -> Double
perimeter []           = 0
perimeter (start : xs) = perimeter_ (start : xs)
 where
  perimeter_ []           = 0
  perimeter_ [x         ] = 0
  perimeter_ (x : y : []) = euclideanDist x y + euclideanDist y start
  perimeter_ (x : y : xs) = euclideanDist x y + perimeter_ (y : xs)

main :: IO ()
main = do
  n  <- readLn :: IO Int
  ns <- replicateM n readTuple
  printf "%.1f" $ perimeter ns