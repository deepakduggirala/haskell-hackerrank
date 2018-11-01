import qualified Data.Map.Strict               as M
import           Data.Maybe

-- import           Control.Monad.State.Lazy
-- import           Test.QuickCheck
import qualified Data.List                     as L

newtype Trie a = Trie (M.Map a (Trie a)) deriving Show

-- find :: Ord a => [a] -> Trie a -> Maybe (Trie a)
-- find []       t        = Just t
-- find [x     ] (Trie m) = M.lookup x m
-- find (x : xs) (Trie m) = M.lookup x m >>= find xs

-- elemIn :: Ord a => [a] -> Trie a -> Bool
-- elemIn xs = isJust . find xs

emptyTrie :: Trie a
emptyTrie = Trie M.empty

splitPrefix :: Ord a => Trie a -> [a] -> ([a], [a])
splitPrefix _        []         = ([], [])
splitPrefix (Trie m) s@(x : xs) = case m M.!? x of
  Nothing -> ([], s)
  Just m' -> liftFst (x :) $ splitPrefix m' xs

insert :: Ord a => [a] -> Trie a -> Trie a
insert []       t        = t
insert (x : xs) (Trie m) = case m M.!? x of
  Nothing -> Trie $ M.insert x (insert xs emptyTrie) m
  Just m' -> Trie $ M.insert x (insert xs m') m


-- flatten :: Trie a -> [[a]]
-- flatten (Trie m) = if M.null m then [[]] else concatMap f (M.toList m)
--  where
--   f :: (a, Trie a) -> [[a]]
--   f (c, m') = map (c :) (flatten m')

-- prefixes :: Ord a => [a] -> Trie a -> [[a]]
-- prefixes xs t = case find xs t of
--   Nothing -> []
--   Just t' -> map (xs ++) (flatten t')

compose :: [a -> a] -> a -> a
compose = foldr (.) id

fromList :: Ord a => [[a]] -> Trie a
fromList = flip (compose . map insert) emptyTrie

main :: IO ()
main =
  interact
    $ unlines
    . map (uncurry solve . liftFst words . tuplify2 . tail)
    . splitEvery 3
    . tail
    . lines

splitEvery :: Int -> [a] -> [[a]]
splitEvery n []   = []
splitEvery n list = first : splitEvery n second
  where (first, second) = L.splitAt n list

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _      = error "list does not have 2 elements"

liftFst :: (a -> c) -> (a, b) -> (c, b)
liftFst f (x, y) = (f x, y)

solve :: [String] -> String -> String
solve passwords attempt =
  maybe "WRONG PASSWORD" unwords $ f (splitPrefix $ fromList passwords) attempt

f :: (String -> (String, String)) -> String -> Maybe [String]
f splitf [] = Just []
f splitf attempt =
  let (password, rest) = splitf attempt
  in  if null password then Nothing else (password :) <$> f splitf rest
