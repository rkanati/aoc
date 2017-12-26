
import KnotHash as KH
import Data.Bits (popCount)

main = do
  let input = "jxqlasbh"
      rowKey i = input ++ "-" ++ show i
      rowKeys = map rowKey [0..127]
      hashes = map KH.hash rowKeys
      n = sum . map (sum . map popCount) $ hashes
  putStrLn.show $ n

