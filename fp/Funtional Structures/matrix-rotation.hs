import           Control.Monad
import qualified Data.Map.Strict               as Map
import qualified Data.List                     as L

readTuple2 :: IO (Int, Int, Int)
readTuple2 = do
  s <- getLine
  let [a, b, c] = map (read :: String -> Int) $ words s
  return (a, b, c)

readRow :: Map.Map (Int, Int) Int -> Int -> IO (Map.Map (Int, Int) Int)
readRow m i = foldr (f i) m . zip [1 ..] . map read . words <$> getLine
 where
  f :: Int -> (Int, Int) -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
  f i (j, a) = Map.insert (i, j) a

-- readRow2 :: Map.Map (Int, Int) Int -> Int -> IO (Map.Map (Int, Int) Int)
-- readRow2 grid i =
--   Map.fromList . (zip (zip (repeat i) [1 ..])) . map read . words <$> getLine

printGrid :: (Int, Int) -> Map.Map (Int, Int) Int -> IO ()
printGrid (m, n) grid = putStr $ unlines $ map f [1 .. m]
 where
  f :: Int -> String
  f i = unwords $ map (show . (Map.!) grid) $ zip (repeat i) [1 .. n]

grid :: Map.Map (Int, Int) Int
grid =
  Map.fromList [ ((i, j), 4 * (i - 1) + j) | i <- [1 .. 4], j <- [1 .. 4] ]


toLineIndex :: (Int, Int) -> (Int, Int) -> Int
toLineIndex (m, n) (i, j) | i == 1          = j - 1
                          | j == n          = i + n - 2
                          | i == m && j < n = m + 2 * n - 2 - j
                          | otherwise       = 2 * n + 2 * (m - 2) - i + 1

fromLineIndex :: (Int, Int) -> Int -> (Int, Int)
fromLineIndex (m, n) k | kn < n             = (1, kn + 1)
                       | kn < n + m - 1     = (kn - n + 2, n)
                       | kn < 2 * n + m - 2 = (m, m + 2 * n - 2 - kn)
                       | otherwise          = (2 * n + 2 * (m - 2) - kn + 1, 1)
 where
  p  = 2 * n + 2 * (m - 2)
  kn = k `rem` p

shift :: (Int, Int) -> Int -> (Int, Int) -> (Int, Int)
shift (m, n) r (i, j) = fromLineIndex (m, n) $ toLineIndex (m, n) (i, j) + r

depth :: (Int, Int) -> (Int, Int) -> Int
depth (m, n) (i, j) = min (min (i - 1) (m - i)) (min (j - 1) (n - j))

translate :: Int -> (Int, Int) -> (Int, Int)
translate d (i, j) = (i - d, j - d)

untranslate :: Int -> (Int, Int) -> (Int, Int)
untranslate d (i, j) = (i + d, j + d)

shiftGrid
  :: (Int, Int) -> Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
shiftGrid (m, n) r grid = Map.mapWithKey f grid
 where
  f :: (Int, Int) -> Int -> Int
  f (i, j) _ =
    (Map.!) grid
      $ untranslate d
      . shift (m - 2 * d, n - 2 * d) r
      . translate d
      $ (i, j)
    where d = depth (m, n) (i, j)

main :: IO ()
main = do
  (m, n, r) <- readTuple2
  grid      <- foldM readRow Map.empty [1 .. m]
  printGrid (m, n) $ shiftGrid (m,n) r grid
  return ()