{-# LANGUAGE BangPatterns #-}

import Data.List (span, unfoldr)
import Data.Char (isDigit, ord, isSpace)
import Data.Word (Word8)
import Data.Foldable (foldl', foldr')
import Data.Bits (xor)
import Numeric (showHex)

step :: (Word8, Word8, [Word8]) -> Word8 -> (Word8, Word8, [Word8])
step (!orig, !skip, !ring) !n = (orig', skip+1, ring') where
  !orig' = orig - n - skip
  !ring' = suff ++ jump
  (jump, suff) = splitAt (fromIntegral skip) $ rest ++ reverse range
  (range, rest) = splitAt (fromIntegral n) ring

hash :: [Word8] -> Int
hash = finish . foldl' step (0, 0, ring0) where
  ring0 = [0..255]
  finish (!orig, _, !ring) =
    product . map fromIntegral . take 2 $ drop (fromIntegral orig) (cycle ring)

hash2 :: String -> String
hash2 input = format . condense . (!! 63) $ unfoldr round (0, 0, ring0) where
  ring0 = [0..255]

  ls :: [Word8]
  ls = map (fromIntegral.ord) input ++ [17, 31, 73, 47, 23]

  round :: (Word8, Word8, [Word8]) -> Maybe ([Word8], (Word8, Word8, [Word8]))
  round (!origin, !skip, !ring) = Just (out, (origin', skip', ring')) where
    !out = take 256 $ drop (fromIntegral origin') (cycle ring')
    (!origin', !skip', !ring') = foldl' step (origin, skip, ring) ls

  condense :: [Word8] -> [Word8]
  condense [] = []
  condense bs = out : condense suff where
    !out = foldr' xor 0 pref
    (!pref, !suff) = splitAt 16 bs

  format :: [Word8] -> String
  format = {-foldr' ($) ""-} concat . map toHex where
    toHex b = case showHex b "" of
      (a:b:[]) -> [a,b]
      (a:[])   -> ['0', a]

parse :: String -> [Word8]
parse "" = []
parse cs = read pref : parse rest where
  rest = dropWhile (not.isDigit) suff
  (pref, suff) = span isDigit cs

main :: IO ()
main = do
  input <- takeWhile (not.isSpace) <$> readFile "input"
  putStrLn.show.hash.parse $ input
  putStrLn.hash2 $ input

