{-# LANGUAGE BangPatterns #-}

module KnotHash (hash) where

import Prelude hiding (round)
import Data.List (unfoldr)
import Data.Char (ord)
import Data.Word (Word8)
import Data.Foldable (foldl', foldr')
import Data.Bits (xor)

hash :: String -> [Word8]
hash input = condense $! (!! 63) $ unfoldr round (0, 0, [0..255]) where
  ls :: [Word8]
  ls = map (fromIntegral.ord) input ++ [17, 31, 73, 47, 23]

  round :: (Word8, Word8, [Word8]) -> Maybe ([Word8], (Word8, Word8, [Word8]))
  round (origin, skip, ring) = Just (out, (origin', skip', ring')) where
    out = take 256 $ drop (fromIntegral origin') (cycle ring')
    (origin', skip', ring') = foldl' step (origin, skip, ring) ls

  step :: (Word8, Word8, [Word8]) -> Word8 -> (Word8, Word8, [Word8])
  step (!orig, !skip, !ring) !n = (orig', skip+1, ring') where
    !orig' = orig - n - skip
    !ring' = suff ++ jump
    (!jump, !suff) = splitAt (fromIntegral skip) $ rest ++ reverse range
    (!range, !rest) = splitAt (fromIntegral n) ring

  condense :: [Word8] -> [Word8]
  condense = map (foldr' xor 0 . take 16) . take 16 . iterate (drop 16)

