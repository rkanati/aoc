
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (First(..), getFirst)

main :: IO ()
main = do
  input <- (map read.words) <$> readFile "input"
  --let input = [ 0, 2, 7, 0 ]
  let ts = traces input
  putStrLn.show $ solve ts done1
  putStrLn.show $ solve ts done2

done1 []       = Nothing
done1 (s : ss) = if s `elem` ss then Just (length ss) else Nothing

done2 []       = Nothing
done2 (s : ss) = fmap (+1) (elemIndex s ss)

solve ts done = fromJust.getFirst.foldMap (First . done) $ ts where

traces input = iterate advance [input] where
  advance (s : ss) = step s : s : ss

  step bs = zipWith (+) remain delta where
    len = length input
    n = maximum bs
    from = fromJust $ elemIndex n bs
    remain = ls++[0]++rs where (ls, _:rs) = splitAt from bs

    trailLength = len - from - 1
    trail = replicate (from + 1) 0 ++ replicate trailLength 1
    leadLength = (n - trailLength) `mod` len
    lead = replicate leadLength 1 ++ replicate (len - leadLength) 0
    wrapCount = (n - leadLength - trailLength) `div` len
    wrap = replicate len wrapCount
    delta = zipWith3 (\a b c -> a+b+c) trail lead wrap

