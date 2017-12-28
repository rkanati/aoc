{-# LANGUAGE BangPatterns #-}

import Data.Bits ((.&.))

gen :: Int -> Int -> Int
gen k i = (k*i) `rem` 2147483647

countTrues :: [Bool] -> Int
countTrues = go 0 where
  go !n [] = n
  go !n (False : bs) = go  n    bs
  go !n (True  : bs) = go (n+1) bs

main :: IO ()
main = do
  let as = tail $ iterate (gen 16807) 679
      {-# INLINE as #-}
      bs = tail $ iterate (gen 48271) 771
      {-# INLINE bs #-}
      als = map (.&. 0xffff) as
      bls = map (.&. 0xffff) bs
      js = zipWith (==) als bls
  putStrLn.show.countTrues.take 40000000 $ js

  let a's = filter (\x -> x `rem` 4 == 0) as
      b's = filter (\x -> x `rem` 8 == 0) bs
      a'ls = map (.&. 0xffff) a's
      b'ls = map (.&. 0xffff) b's
      j's = zipWith (==) a'ls b'ls
  putStrLn.show.countTrues.take 5000000 $ j's

