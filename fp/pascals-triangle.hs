import           Control.Monad

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)


pascalsValue :: Int -> Int -> Int
pascalsValue n r = ((fact n) `div` (fact r * fact (n - r)))

pascalsLine :: Int -> IO ()
pascalsLine n = putStrLn . unwords $ map f [0 .. n]
  where f = show . (pascalsValue n)

pascalsTraingle :: Int -> IO ()
pascalsTraingle n = mapM_ pascalsLine [0 .. (n - 1)]

main :: IO ()
main = do
  n <- readLn :: IO Int
  pascalsTraingle n