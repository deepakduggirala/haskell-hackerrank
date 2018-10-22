import           Control.Monad
import           Data.List                      ( intercalate )

fib' :: (Int -> Integer) -> Int -> Integer
fib' f 0 = 0
fib' f 1 = 1
fib' f n = f (n - 1) + f (n - 2)

f_list :: [Integer]
f_list = map (fib' faster_f) [0 ..]

faster_f n = f_list !! n

solve :: Int -> Integer
solve x = (faster_f x) `rem` 100000007


main :: IO ()
main = interact $ intercalate "\n" . map (show . solve . read) . tail . words