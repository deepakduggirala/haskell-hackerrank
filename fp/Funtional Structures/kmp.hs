-- https://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell

main :: IO ()
main = interact $ unlines . map (uncurry solve) . split2 . tail . lines

makeTuple :: [Int] -> (Int, Int)
makeTuple [x, y] = (x, y)

split2 :: [String] -> [(String, String)]
split2 []           = []
split2 (x : y : xs) = (x, y) : (split2 xs)

solve :: String -> String -> String
solve haystack needle = case kmp needle haystack of
  True  -> "YES"
  False -> "NO"

data KMP a = KMP
  { done :: Bool
  , next :: (a -> KMP a)
  }

kmp :: Eq a => [a] -> [a] -> Bool
kmp as bs = match (makeTable as) bs
 where
  match table []       = done table
  match table (b : bs) = done table || match (next table b) bs

makeTable :: Eq a => [a] -> KMP a
makeTable xs = table where table = makeTable' xs (const table)

makeTable' []       failure = KMP True failure
makeTable' (x : xs) failure = KMP False test
 where
  test c = if c == x then success else failure c
  success = makeTable' xs (next (failure x))
