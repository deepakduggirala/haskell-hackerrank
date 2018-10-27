main :: IO ()
main =
  interact
    $ unlines
    . map (show . divisors . uncurry gcd . makeTuple . map read . words)
    . tail
    . lines

makeTuple :: [Int] -> (Int, Int)
makeTuple [x, y] = (x, y)

divisors :: Int -> Int
divisors n =
  let
    d = [ i | i <- [1 .. srFloor], n `mod` i == 0 ]
  in
    if isPerfectSquare then (2*length d) - 1 else 2* length d
  where
    sr = sqrt (fromIntegral  n) :: Double
    srFloor = floor sr :: Int
    isPerfectSquare =  sr == fromIntegral srFloor