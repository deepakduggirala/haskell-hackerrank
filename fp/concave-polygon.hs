import           Control.Monad
import           Data.List                      ( maximumBy
                                                , minimumBy
                                                , sortBy
                                                )

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

h :: [Int] -> (Int, Int)
h (x : y : []) = (x, y)

readTuple :: IO (Int, Int)
readTuple = do
  s <- getLine
  return $ h (map read $ words s)

getPoints :: Int -> IO [(Int, Int)]
getPoints n = replicateM n readTuple

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

rightTurn :: [Point] -> Bool
rightTurn (p1 : p2 : p3 : ps) = case orientation p1 p2 p3 of
  Counterclockwise -> rightTurn (p2 : p3 : ps)
  Clockwise        -> True
  Collinear        -> rightTurn (p2 : p3 : ps)
rightTurn _ = False

orderByPolarAngel :: Point -> [Point] -> [Point]
orderByPolarAngel p = sortBy (polarAngleCompare p)


concaveScan :: [Point] -> Bool
concaveScan ps = rightTurn $ orderByPolarAngel start ps
  where start = (bottomMost ps)

main :: IO ()
main = do
  n      <- readLn :: IO Int
  points <- getPoints n
  putStrLn $ if concaveScan points then "YES" else "NO"
  return ()