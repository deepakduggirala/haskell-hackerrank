getNumStr :: String -> String
getNumStr = show . length

compress :: String -> String
compress []       = []
compress (x : xs) = case span (== x) xs of
  ([], _ ) -> x : compress xs
  (s1, s2) -> x : getNumStr (x : s1) ++ compress s2

main :: IO ()
main = do
  s <- getLine
  putStrLn (compress s)