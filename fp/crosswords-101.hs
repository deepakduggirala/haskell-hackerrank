import           Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict               as M
import           Data.Bifunctor
import           Control.Applicative

splitStr :: Char -> String -> [String]
splitStr c []       = []
splitStr c (x : xs) = if c == x
  then "" : splitStr c xs
  else case splitStr c xs of
    []       -> [[x]]
    (r : rs) -> (x : r) : rs


type Pos = (Int, Int)
type Range = (Pos, Int, Orientation)

data Letter = Blocked | Unfilled | Filled Char deriving (Show, Eq)
data Orientation = Vertical | Horizontal deriving (Show, Eq, Ord)

data Crossword = Crossword {
  grid :: M.Map Pos Letter,
  vacancy :: M.Map Int [Range]
} deriving (Show)

ranges :: Crossword -> String -> [Range]
ranges cw word = filter (verify cw word) vacancies
  where vacancies = fromMaybe [] (M.lookup (length word) (vacancy cw))

insert :: String -> Crossword -> [Crossword]
insert word cw = map (set cw word) (ranges cw word)

insertWords :: [String] -> Crossword -> [Crossword]
insertWords words cw = foldM (flip insert) cw words

buildCrossword :: [String] -> Crossword
buildCrossword text =
  let gridList = map (\x -> zip (repeat x) [1 ..]) [1 ..]
      letterGrid =
        filter ((== Unfilled) . snd)
          . map (second toLetter)
          . concat
          . zipWith zip gridList
          $ text
      gridM = M.fromList letterGrid
  in  Crossword gridM (buildVacancyMap gridM)

buildVacancyMap :: M.Map Pos Letter -> M.Map Int [Range]
buildVacancyMap gridM =
  let positions = M.keys gridM
      lku       = isJust . flip M.lookup gridM
      limit     = length . takeWhile lku
      directions (x, y) = explore (lku (x, y - 1)) (lku (x - 1, y))
      possibleRanges :: Pos -> [(Int, Orientation)]
      possibleRanges pos =
        filter ((> 1) . fst) . map (first limit . ($ pos)) $ directions pos
      rearrange :: Pos -> (Int, Orientation) -> (Int, [Range])
      rearrange pos (l, o) = (l, [(pos, l, o)])
      vacancyList =
        concatMap (\pos -> map (rearrange pos) . possibleRanges $ pos) positions
  in  M.fromListWith (++) vacancyList

showCrossword :: Crossword -> String
showCrossword cw =
  let gridList = map (\x -> zip (repeat x) [1 .. 10]) [1 .. 10]
  in  unlines . map (map (fromLetter . lookupLetter cw)) $ gridList


exploreH (px, py) = (map (\y -> (px, y)) [py ..], Horizontal)
exploreV (px, py) = (map (\x -> (x, py)) [px ..], Vertical)
-- explore elemOnLeft elemOnTop
explore :: Bool -> Bool -> [Pos -> ([Pos], Orientation)]
explore False False = [exploreH, exploreV]
explore False True  = [exploreH]
explore True  False = [exploreV]
explore True  True  = []



toLetter :: Char -> Letter
toLetter '-' = Unfilled
toLetter '+' = Blocked
toLetter c   = Filled c

fromLetter :: Letter -> Char
fromLetter Blocked    = '+'
fromLetter (Filled c) = c
fromLetter Unfilled   = '-'

lookupLetter :: Crossword -> Pos -> Letter
lookupLetter cw p = fromMaybe Blocked $ M.lookup p (grid cw)

verify :: Crossword -> String -> Range -> Bool
verify cw s r = match (get cw r) s

match :: [Letter] -> String -> Bool
match []       []       = True
match []       _        = False
match _        []       = False
match (f : fs) (w : ws) = case f of
  Blocked  -> False
  Unfilled -> match fs ws
  Filled c -> c == w && match fs ws

get :: Crossword -> Range -> [Letter]
get cw r = map (lookupLetter cw) (range r)

set :: Crossword -> String -> Range -> Crossword
set cw word r = foldl f cw (zip word $ range r)
  where
  f :: Crossword -> (Char, Pos) -> Crossword
  f cw (c, pos) = cw { grid = M.insert pos (Filled c) (grid cw) }

range :: Range -> [Pos]
range ((sx, sy), l, orientation) = map
  (\x -> if orientation == Vertical then (sx + x, sy) else (sx, sy + x))
  [0 .. l - 1]

main :: IO ()
main = interact $ uncurry f . splitAt 10 . lines
  where
  f :: [String] -> [String] -> String
  f text wordsStrList =
    let cw = buildCrossword text
        ws = splitStr ';' . head $ wordsStrList
    in  showCrossword . head . insertWords ws $ cw


-- Possible improvments: insert words in the order of increasing vacancy possibilities.
-- If the grid has space to fill three 4 letter words, two 3 letter and one 6 letter start
--  with inserting 6 letter word, 3 letter and finally 4 letter words
