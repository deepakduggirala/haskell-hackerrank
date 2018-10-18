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

area :: [Point] -> Double
area []           = 0
area (start : xs) = area_ (start : xs)
 where
  area_ []             = 0
  area_ [p           ] = (areaLineSegment p start)
  area_ (p1 : p2 : ps) = (areaLineSegment p1 p2) + area_ (p2 : ps)

areaLineSegment :: Point -> Point -> Double
areaLineSegment (x1, y1) (x2, y2) =
  fromIntegral (x2 * y2 - x1 * y1 + y1 * x2 - x1 * y2) / 2

main :: IO ()
main = do
  n  <- readLn :: IO Int
  ns <- replicateM n readTuple
  printf "%.1f" $ abs $ area ns