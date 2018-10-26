import qualified Data.Map.Strict               as M
import           Data.Function                  ( on )

main :: IO ()
main =
  interact
    $ showFactorization
    . listGCD
    . map (makeMap . map read . words)
    . tail
    . lines

makeMap :: [Int] -> M.Map Int Int
makeMap []           = M.empty
makeMap (p : x : ns) = M.insert p x (makeMap ns)

listGCD :: [M.Map Int Int] -> M.Map Int Int
listGCD [m     ] = m
listGCD (m : ms) = M.intersectionWith min m (listGCD ms)

showFactorization :: M.Map Int Int -> String
showFactorization =
  unwords . map (\(x, y) -> show x ++ " " ++ show y) . M.toAscList