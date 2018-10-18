import           Control.Monad

swapEven :: [a] -> [a]
swapEven []             = []
swapEven [x           ] = [x]
swapEven (x1 : x2 : xs) = x2 : x1 : swapEven xs

main :: IO ()
main = do
  n <- readLn :: IO Int
  replicateM_
    n
    (do
      s <- getLine
      putStrLn (swapEven s)
    )