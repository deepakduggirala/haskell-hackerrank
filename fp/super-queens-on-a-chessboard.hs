import           Control.Monad
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.List
import           Data.Function

data Square = Occupied | Unavailable deriving Show
type Pos = (Int, Int)
type Board = M.Map Int [Int]

emptyBoard :: Int -> Board
emptyBoard n = M.empty

superNQueens :: Int -> Board -> Int -> [Board]
superNQueens n board 1 = map (\r -> set n (r, 1) board) [1 .. n]
superNQueens n board c = [ set n (r, c) board | r <- getColumn n c board ]

getColumn n c board = fromMaybe [1 .. n] (M.lookup c board)

set :: Int -> Pos -> Board -> Board
set n pos@(r, c) board = foldr
  (\(c, rs) b -> M.insert c (getColumn n c b \\ rs) b)
  board
  (threatenedPos n r c)

threatenedPos n r c =
  map f
    .  groupBy (on (==) snd)
    .  sortOn snd
    $  [ (r, y) | y <- [(c + 1) .. n] ]
    ++ [ (r + i, c + i) | i <- [1 .. n - max r c] ]
    ++ [ (r - i, c + i) | i <- [1 .. min (r - 1) (n - c)] ]
    ++ filter (\(x, y) -> isBetween 0 n x && isBetween 0 n y)
              [(r - 1, c + 2), (r + 1, c + 2), (r + 2, c + 1), (r - 2, c + 1)]
  where
  f :: [(Int, Int)] -> (Int, [Int])
  f l@(x : xs) = (snd x, map fst l)

isBetween :: Int -> Int -> Int -> Bool
isBetween l h x = l <= x && x <= h

-- showBoard :: Board -> Int -> String
-- showBoard board n =
--   unlines . map (\r -> [ getSquare n r c board | c <- [1 .. n] ]) $ [1 .. n]

-- getSquare :: Int -> Int -> Int -> Board -> Char
-- getSquare n r c board =
--   maybe '-' (const 'x') $ find (== r) $ fromMaybe [1 .. n] (M.lookup c board)

-- fromSquare :: Maybe Square -> Char
-- fromSquare Nothing            = '-'
-- fromSquare (Just Occupied   ) = 'o'
-- fromSquare (Just Unavailable) = 'x'

solve :: Int -> Int
solve n = length $ foldM sq (emptyBoard n) [1 .. n] where sq = superNQueens n

main :: IO ()
main = interact $ show . solve . read

-- s n b = putStr (showBoard b n)
