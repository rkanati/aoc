
main :: IO ()
main = putStrLn.show.spiralDist $ 312051

spiralDist i = ring + offCardinal where
  -- how far are we around the ring from the nearest cardinal?
  offCardinal = abs $ (turn `mod` (2*ring)) - ring

  -- how far round from the highest end of the ring are we?
  turn = ringMax - i

  -- what's the highest number in our ring?
  ringMax = let d = 2*ring + 1 in d*d

  -- which square "ring" is this address in?
  ring = diam `div` 2 where
    ri = ceiling (sqrt.realToFrac $ i)
    diam = if even ri then ri+1 else ri

