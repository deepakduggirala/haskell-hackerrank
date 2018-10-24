insideTraingle :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
insideTraingle (p0x, p0y) (p1x, p1y) (p2x, p2y) (px, py) =
  let
    s'        = p0y * p2x - p0x * p2y + (p2y - p0y) * px + (p0x - p2x) * py
    t'        = p0x * p1y - p0y * p1x + (p0y - p1y) * px + (p1x - p0x) * py
    areaTwice = -p1y * p2x + p0y * (-p1x + p2x) + p0x * (p1y - p2y) + p1x * p2y
  in
    s' >= 0 && t' >= 0 && s' + t' <= areaTwice

triangle :: (Int, Int) -> Int -> Int -> (Int, Int) -> Bool
triangle a 1 1 = (== a)
triangle a@(x, y) height base =
  let b = (x - base `div` 2, y - height + 1)
      c = (x + base `div` 2, y - height + 1)
  in  insideTraingle b c a

display :: ((Int, Int) -> Bool) -> [String]
display f = map (concatMap (d . f))
                [ zip [1 .. 63] (replicate 63 col) | col <- [32, 31 .. 1] ]
d True  = "1"
d False = "_"

any' :: [a -> Bool] -> a -> Bool
any' ps = or . (ps <*>) . pure

sierpinski :: (Int, Int) -> Int -> Int -> Int -> (Int, Int) -> Bool
sierpinski a        h b 0 = triangle a h b
sierpinski a@(x, y) h b n = any'
  [ sierpinski a  h2 b2 (n - 1)
  , sierpinski ab h2 b2 (n - 1)
  , sierpinski ac h2 b2 (n - 1)
  ]
  where
  b2 = b `div` 2
  h2 = h `div` 2
  ab = (x - b `div` 4 - 1, y - h2)
  ac = (x + b `div` 4 + 1, y - h2)

a = (32, 32)
base = 63
height = 32

main :: IO ()
main = interact $ unlines . display . (sierpinski a height base) . read
