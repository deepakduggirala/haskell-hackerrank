module Throwaway where

  import           Control.Monad
  import           Data.Array
  
  data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show
  
  instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
  
  inorder :: Tree a -> [a]
  inorder Empty        = []
  inorder (Node l x r) = inorder l ++ [x] ++ inorder r
  
  n x = Node Empty x Empty
  t = Node (n 2) 1 (n 3)
  
  depthMap :: (Int -> a -> b) -> Tree a -> Tree b
  depthMap f = g 1
   where
    g d Empty        = Empty
    g d (Node l x r) = Node (g (d + 1) l) (f d x) (g (d + 1) r)
  
  swapNode :: Tree (a, Bool) -> Tree (a, Bool)
  swapNode Empty = Empty
  swapNode (Node l x r) =
    let ln = swapNode l
        rn = swapNode r
    in  if snd x then Node rn x ln else Node ln x rn
  
  constructTree :: Int -> Array Int (Int, Int) -> Tree Int
  constructTree n a
    | n == -1 = Empty
    | otherwise = Node (constructTree (fst $ a ! n) a)
                       n
                       (constructTree (snd $ a ! n) a)
  
  h :: [Int] -> (Int, Int)
  h (x : y : []) = (x, y)
  
  readTuple :: IO (Int, Int)
  readTuple = do
    s <- getLine
    return $ h (map read $ words s)
  
  f :: Tree Int -> Int -> IO (Tree Int)
  f tree k =
    let
      swapped_tree = swapNode $ depthMap (\d a -> (a, (d `rem` k)==0)) tree
      swapped_tree_clean = fmap fst swapped_tree
      inorder_str = unwords . (map show) $ inorder swapped_tree_clean
    in
      do
        putStrLn inorder_str
        return swapped_tree_clean
  
  main :: IO ()
  main = do
    n  <- readLn :: IO Int
    ns <- replicateM n readTuple
    t  <- readLn :: IO Int
    ks <- replicateM t (readLn :: IO Int)
    let t = constructTree 1 $ listArray (1, n) ns
    foldM_ f t ks
  