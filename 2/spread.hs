
import Data.Maybe (fromJust)
import Data.List (sort, tails)
import Data.Foldable (fold)
import Data.Monoid (First(..), getFirst)

main :: IO ()
main = do
  input <- readFile "input"
  let table = map (map read . words) . lines $ input
  putStrLn.show.checksum $ table
  putStrLn.show.solve2   $ table

checksum :: [[Integer]] -> Integer
checksum = sum . map rowCheck where
  rowCheck row = maximum row - minimum row

solve2 :: [[Integer]] -> Integer
solve2 = sum . map rowSolve where
  rowSolve = fromJust.getFirst.fold.map tryPairFrom.tails.sort
  tryPairFrom (c:cs) = foldMap (tryPair c) cs
  tryPairFrom _      = First Nothing
  tryPair a b = First $ if gcd a b == a then Just (b `div` a) else Nothing

