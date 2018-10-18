mergeStrings :: [a] -> [a] -> [a]
mergeStrings []       []       = []
mergeStrings []       q        = q
mergeStrings p        []       = p
mergeStrings (p : ps) (q : qs) = p : (q : (mergeStrings ps qs))

main :: IO ()
main = do
  p <- getLine
  q <- getLine
  putStrLn (mergeStrings p q)
