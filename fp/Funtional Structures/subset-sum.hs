
import qualified Data.Map.Strict               as M
import           Data.Function                  ( on )
import           Data.List                      ( sort )

-- https://www.cmi.ac.in/~madhavan/courses/prog2-2012/lectures/balanced-search-trees-in-haskell.txt

liftSnd :: (b -> c) -> (a, b) -> (a, c)
liftSnd f (x, y) = (x, f y)

data Tree a = Empty | Node Int (Tree a) a (Tree a)

instance (Show a) => Show (Tree  a) where
  show = showTree

instance Functor Tree where
  -- fmap :: (a->b) -> Tree a -> Tree b
  fmap f Empty = Empty
  fmap f (Node h l v r) = Node h (fmap f l) (f v) (fmap f r)


showTree Empty            = ""
showTree n@(Node i _ _ _) = go i n
 where
  go _ Empty = ""
  go i (Node _ l c r) =
    go (i - 1) l
      ++ replicate (4 * fromIntegral i) ' '
      ++ show c
      ++ "\n"
      ++ go (i - 1) r

consTree :: Ord a => a -> Tree a -> Tree a
consTree x Empty = Node 1 Empty x Empty
consTree x t@(Node h l v r) | x < v  = rebalance (Node h (consTree x l) v r)
                            | x > v  = rebalance (Node h l v (consTree x r))
                            | x == v = t

rebalance :: Tree a -> Tree a
rebalance Empty = Empty
rebalance t@(Node h t1 y t2)
  | abs sy < 2           = updateHeight t
  | sy == 2 && st1 /= -1 = rotateright t
  | sy == 2 && st1 == -1 = rotateright (Node 0 (rotateleft t1) y t2)
  | sy == -2 && st2 /= 1 = rotateleft (Node 0 t1 y t2)
  | sy == -2 && st2 == 1 = rotateleft (Node 0 t1 y (rotateright t2))
 where
  sy  = slope t
  st1 = slope t1
  st2 = slope t2

  rotateright (Node _ (Node _ ll y lr) x r) =
    let r' = updateHeight (Node 0 lr x r) in updateHeight (Node 0 ll y r')

  rotateleft (Node _ l x (Node _ rl y rr)) =
    let l' = updateHeight (Node 0 l x rl) in updateHeight (Node 0 l' y rr)

slope :: (Tree a) -> Int
slope Empty            = 0
slope (Node _ t1 x t2) = (height t1) - (height t2)

updateHeight :: Tree a -> Tree a
updateHeight Empty            = Empty
updateHeight (Node _ l val r) = Node (max (height l) (height r) + 1) l val r

height :: (Tree a) -> Int
height Empty            = 0
height (Node m t1 x t2) = m

fromList :: Ord a => [a] -> Tree a
fromList = foldr consTree Empty

indexTree :: Tree a -> (Tree (a, Int), Int)
indexTree Empty = (Empty, 0)
indexTree (Node h l v r) =
  let (l', il) = indexTree l
      i        = il + 1
      (r', ir) = indexTree r
      r''      = fmap (liftSnd (+ i)) r'
  in  (Node h l' (v, i) r'', il + ir + 1)

findInTree :: (Eq a, Ord a) => Tree (a, Int) -> a -> (Int, Ordering)
findInTree Empty _ = (0, EQ)
findInTree t@(Node h l v r) x
  | x == fst v = (snd v, EQ)
  | x > fst v  = if nullTree r then (snd v, GT) else findInTree r x
  | x < fst v  = if nullTree l then (snd v, LT) else findInTree l x

nullTree :: Tree a -> Bool
nullTree Empty = True
nullTree _     = False

solve :: [Int] -> [Int] -> [Int]
solve as ss =
  let (sortedAs, l) = indexTree . fromList $ cumulativeFold (sort as)
  in  map (g . f l sortedAs) ss

cumulativeFold = foldr sumAppend []
 where
  sumAppend x []       = [x]
  sumAppend x (a : as) = (a + x) : a : as

f :: Int -> Tree (Int, Int) -> Int -> Maybe Int
f l t x = case findInTree t x of
  (i, LT) -> Just i
  (i, EQ) -> Just i
  (i, GT) -> if i >= l then Nothing else Just (i + 1)

g :: Maybe Int -> Int
g Nothing  = -1
g (Just x) = x

main :: IO ()
main = interact $ unlines . map show . uncurry solve . readInput . tail . lines

readInput :: [String] -> ([Int], [Int])
readInput (x : _ : xs) = (map read . words $ x, map read xs)

