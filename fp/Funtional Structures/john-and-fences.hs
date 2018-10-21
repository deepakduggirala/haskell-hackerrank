import           Control.Monad

-- https://stackoverflow.com/a/50651622/2580077

data CartesianTree a = Empty | Node (CartesianTree a) a (CartesianTree a) deriving (Show)

updateTree :: Ord a => CartesianTree a -> a -> CartesianTree a
updateTree t@(Node l m r) x =
  if m > x then Node t x Empty else Node l m (updateTree r x)
updateTree Empty x = Node Empty x Empty

inorder :: CartesianTree a -> [a]
inorder Empty        = []
inorder (Node l x r) = inorder l ++ [x] ++ (inorder r)

cTfromList :: Ord a => [a] -> CartesianTree a
cTfromList = foldl updateTree Empty

prop_tree :: Ord a => [a] -> Bool
prop_tree x = inorder (cTfromList x) == x

largestRect :: (Int, Int) -> CartesianTree (Int, Int) -> Int
largestRect (lb, rb) (Node lt (m, ix) rt) = maximum
  [m * (rb - lb + 1), largestRect (lb, ix - 1) lt, largestRect (ix + 1, rb) rt]
largestRect _ Empty = 0

solve :: [Int] -> Int
solve x = largestRect (0, length x - 1) (cTfromList $ zip x [0 ..])


main :: IO ()
main = interact $ show . solve . map read . tail . words