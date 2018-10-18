reduce :: String -> String
reduce []       = []
reduce (x : xs) = x : (filter (/= x) (reduce xs))


main :: IO ()
main = do
  s1 <- getLine
  putStrLn $ reduce s1