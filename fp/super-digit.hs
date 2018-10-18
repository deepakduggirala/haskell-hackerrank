import           Control.Monad
h :: [String] -> (String, Int)
h (x : y : []) = (x, read y)

f :: Int -> Int
f x = let n = x `rem` 9 in if n == 0 then 9 else n

superDigit :: Int -> String -> Int
superDigit s []       = s
superDigit s (n : ns) = superDigit (f $ s + read [n]) ns

main :: IO ()
main = do
  contents <- getContents
  let (n, k) = h . words $ contents
  print $ f $ (superDigit 0 n) * k
  return ()