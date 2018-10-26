limit :: Int -> Int -> Int
limit x n = ceiling $ fromIntegral x ** (1 / fromIntegral n)

makeTuple :: [Int] -> (Int, Int)
makeTuple [x, y] = (x, y)

solve :: (Int, Int) -> Int
solve (x, n) =
  length $ filter (== x) $ subsetSum x [ i ^ n | i <- [1 .. limit x n] ]

main :: IO ()
main = interact $ show . solve . makeTuple . map read . words

subsetSum :: Int -> [Int] -> [Int]
subsetSum _ []       = [0]
subsetSum n (x : xs) = ssxs ++ filter (<= n) (map (x +) ssxs)
  where ssxs = subsetSum n xs
