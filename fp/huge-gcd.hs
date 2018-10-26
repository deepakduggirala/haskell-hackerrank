import qualified Data.Map.Strict               as M
import           Data.Function                  ( on )

-- http://www.cut-the-knot.org/blue/gcd_fta.shtml

primes100 =
  [ 2
  , 3
  , 5
  , 7
  , 11
  , 13
  , 17
  , 19
  , 23
  , 29
  , 31
  , 37
  , 41
  , 43
  , 47
  , 53
  , 59
  , 61
  , 67
  , 71
  , 73
  , 79
  , 83
  , 89
  , 97
  ]

factorize = factorize' primes100

factorize' :: [Int] -> Int -> M.Map Int Int
factorize' _        1 = M.empty
factorize' []       n = M.insertWith (+) n 1 M.empty
factorize' (p : ps) n = M.insertWith (+) p x (factorize' ps y)
  where (x, y) = repeatedDivision n p

repeatedDivision :: Int -> Int -> (Int, Int)
repeatedDivision n p =
  case takeWhile (\x -> x /= 0 && x `mod` p == 0) (iterate (`div` p) n) of
    [] -> (0, n)
    x  -> (length x, last x `div` p)

factorizeList :: [Int] -> M.Map Int Int
factorizeList = foldr (M.unionWith (+) . factorize) M.empty

hugeGCD :: [Int] -> [Int] -> M.Map Int Int
hugeGCD = on (M.intersectionWith min) factorizeList


modProduct :: Int -> M.Map Int Int -> Int
modProduct p = M.foldlWithKey (\m b e -> (m * expMod b e p) `mod` p) 1

expMod :: Int -> Int -> Int -> Int
expMod b e m | e == 0    = 1
             | odd e     = b `mod` m * expMod b (e - 1) m `mod` m
             | otherwise = expMod ((b * b) `mod` m) (e `div` 2) m

solve :: [Int] -> [Int] -> Int
solve x y = modProduct 1000000007 (hugeGCD x y)

main :: IO ()
main = do
  getLine
  x <- map read . words <$> getLine
  getLine
  y <- map read . words <$> getLine
  print . solve x $ y

first' :: Int -> [Int] -> Int
first' n []       = 1
first' n (p : ps) = if n `mod` p == 0 then p else first' n ps