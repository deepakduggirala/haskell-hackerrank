
import qualified Data.Map.Strict               as M
import           Data.Function                  ( on )
import           Data.List                      ( sortBy
                                                , minimumBy
                                                , break
                                                )
import           Data.Ord                       ( comparing )

-- https://www.hackerrank.com/challenges/mirko-at-construction-site/problem

sndOf3 :: (a, b, c) -> b
sndOf3 (a, b, c) = b

fstOf4 :: (a, b, c, d) -> a
fstOf4 (a, b, c, d) = a

liftFst :: (a -> c) -> (a, b) -> (c, b)
liftFst f (x, y) = (f x, y)

solve :: ([Double], [Double], [Int]) -> [Int]
solve (cs, ms, qs) =
  let lines     = zip3 cs ms [1 ..]
      intervals = buildIntervals (maximum lines) lines
  in  map (findInterval intervals) qs

findInterval :: [(Double, Double, Double, Int)] -> Int -> Int
findInterval intervals q =
  case break ((>= fromIntegral q) . fstOf4) intervals of
    (_, [(x, _, _, i)]) -> i
    (_, (x1, c1, m1, i1) : (x2, c2, m2, i2) : ps) ->
      if (q == floor x1) && (y c1 m1 q == y c2 m2 q) then max i1 i2 else i1
      where y c m q = floor c + floor m * q

buildIntervals
  :: (Double, Double, Int)
  -> [(Double, Double, Int)]
  -> [(Double, Double, Double, Int)]
buildIntervals (_, _, _) []          = []
buildIntervals (_, _, _) [(c, m, i)] = [(1 / 0, c, m, i)]
buildIntervals (cmax, m_cmax, imax) lines
  | null (eligibleLines m_cmax lines)
  = [(1 / 0, cmax, m_cmax, imax)]
  | otherwise
  = let elgLines = eligibleLines m_cmax lines
        its (c, m, _) = (cmax - c) / (m - m_cmax)
        (cf, mf, i_f) = minimumBy (comparing its) elgLines
        x_next        = its (cf, mf, 0)
    in  (x_next, cmax, m_cmax, imax) : buildIntervals (cf, mf, i_f) elgLines

eligibleLines m = filter ((> m) . sndOf3)

main :: IO ()
main =
  interact $ unlines . map show . solve . readInput . splitAt 2 . tail . lines

readInput :: ([String], [String]) -> ([Double], [Double], [Int])
readInput ([cs, ms], qs) =
  (map read . words $ cs, map read . words $ ms, map read qs)

