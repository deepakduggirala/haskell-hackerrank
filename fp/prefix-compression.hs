outFormat :: String -> String
outFormat s = ((show . length) s) ++ " " ++ s

compress :: String -> String -> (String, String, String)
compress []         []         = ([], [], [])
compress []         s2         = ([], [], s2)
compress s1         []         = ([], s1, [])
compress (s1 : s1s) (s2 : s2s) = if s1 == s2
  then
    let (substr, s1Rem, s2Rem) = compress s1s s2s
    in  (s1 : substr, s1Rem, s2Rem)
  else ([], (s1 : s1s), (s2 : s2s))


main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  let (substr, s1Rem, s2Rem) = compress s1 s2 in
    do
      putStrLn $ outFormat substr
      putStrLn $ outFormat s1Rem
      putStrLn $ outFormat s2Rem