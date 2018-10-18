import Text.Printf
import           Data.List                      ( maximumBy
                                                , minimumBy
                                                , sortBy
                                                )
import           Data.Ord                       ( comparing
                                                , Ord
                                                )
import Control.Monad

type Point = (Int, Int)

data Orientation = Clockwise | Counterclockwise | Collinear deriving (Show, Eq)
distSq :: Point -> Point -> Int
distSq (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

orientation :: Point -> Point -> Point -> Orientation
orientation (x1, y1) (x2, y2) (x3, y3) =
  let m = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
  in  if m < 0
        then Counterclockwise
        else if m == 0 then Collinear else Clockwise


bottomMost :: [Point] -> Point
bottomMost = minimumBy f
 where
  f (x1, y1) (x2, y2) = case compare y1 y2 of
    EQ -> compare x1 x2
    x  -> x

polarAngleCompare :: Point -> Point -> Point -> Ordering
polarAngleCompare p p1 p2 = case orientation p p1 p2 of
  Counterclockwise -> LT
  Clockwise        -> GT
  Collinear        -> compare (distSq p p1) (distSq p p2)

orderByPolarAngel :: Point -> [Point] -> [Point]
orderByPolarAngel p = sortBy (polarAngleCompare p)

build :: [Point] -> [Point] -> [Point]
build hull []             = hull
build []   [p           ] = [p]
build [h]  [p           ] = [p, h]
build []   (p1 : p2 : ps) = build [p2, p1] ps
build [h]  (p       : ps) = build [p, h] ps
build hull@(h2 : h1 : hs) points@(p : ps)
  | orientation h1 h2 p == Counterclockwise = build (p : hull) ps
  | otherwise                               = build (h1 : hs) points

filterCollinear :: Point -> [Point] -> [Point]
filterCollinear p []  = []
filterCollinear p [x] = [x]
filterCollinear p (x : y : xs)
  | x == p                         = x : filterCollinear p (y : xs)
  | orientation p x y == Collinear = filterCollinear p (y : xs)
  | otherwise                      = x : filterCollinear p (y : xs)

grahamScan :: [Point] -> [Point]
grahamScan ps = build [] (filterCollinear start (orderByPolarAngel start ps))
  where start = (bottomMost ps)

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

solve :: [(Int, Int)] -> Double
solve  = perimeter

h :: [Int] -> (Int, Int)
h xs = (head xs, xs !! 1)

readTuple :: IO (Int, Int)
readTuple = do
  s <- getLine
  return $ h (map read $ words s)

getPoints :: Int -> IO [(Int, Int)]
getPoints n = replicateM n readTuple

main :: IO ()
main = do
  n      <- readLn :: IO Int
  points <- getPoints n
  printf "%.1f\n" $ perimeter . grahamScan $ points