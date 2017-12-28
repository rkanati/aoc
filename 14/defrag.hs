{-# LANGUAGE BangPatterns #-}

import qualified KnotHash as KH
import qualified UF
import Data.Bits (FiniteBits, popCount, testBit, finiteBitSize)
import Data.List (foldl', mapAccumL)
import Data.Foldable (traverse_)

data Run = Run { first, last :: !Int } deriving (Show)

size :: Run -> Int
size (Run f l) = l - f + 1

adjacent :: Run -> Run -> Bool
adjacent (Run fa la) (Run fb lb) = lb >= fa && fb <= la

type Row = [Run]

countRegions :: [Row] -> Int
countRegions rs = UF.size uf where
  uf = foldl' joinRows uf0 adjRows where
    uf0 = foldl' (\u i -> fst $ UF.insert u i) UF.empty [0 .. n0 - 1]
    (n0, namedRows) = mapAccumL (mapAccumL nameSpan) 0 rs where
      nameSpan !i !s = (i + 1, (i, s))
    adjRows = zip ([] : namedRows) (namedRows ++ [[]]) where
    joinRows uf (ss, ts) = foldl' (\u (i,j) -> UF.join u i j) uf pairs where
      pairs = [ (i,j) | (i,s) <- ss, (j,t) <- ts, s `adjacent` t ]

bitsOf :: FiniteBits a => a -> [Bool]
bitsOf a = map (testBit a) [n-1, n-2 .. 0] where n = finiteBitSize a

makeRow :: FiniteBits a => [a] -> Row
makeRow = snd . foldl' step (False, []) . zip [0..] . concat . map bitsOf where
  step (!prev, !runs) (!pos, !cur) = (cur, runs') where
    runs' = if cur then
              case (prev, runs) of
                (True, Run f l : rs) -> Run f (l + 1) : rs
                (_,              rs) -> Run pos pos   : rs
            else
              runs

main :: IO ()
main = do
  let input = "jxqlasbh"
      rowKey :: Int -> String
      rowKey i = input ++ "-" ++ show i
      hashes = map (KH.hash . rowKey) [0..127]
      n = sum . map (sum . map popCount) $ hashes
  putStrLn $ "Squares used: " ++ show n

  let rows = map makeRow hashes
  putStrLn.show $ "Contiguous regions: " ++ show (countRegions rows)

