
import Data.Traversable (mapAccumL)
import Data.Foldable (traverse_)

main :: IO ()
main = putStrLn . show . head . dropWhile (< 312051) $ cells

cells :: [Int]
cells = snd $ mapAccumL step (initial, (1,0)) stepDirs where
  initial = [((0,0), 1)]

  turn (1,0)=(0,1)
  turn (0,1)=(-1,0)
  turn (-1,0)=(0,-1)
  turn (0,-1)=(1,0)

  legLengths = 1 : ([2..] >>= replicate 2)
  legDirs = iterate turn (0,1)
  stepDirs = concat $ zipWith replicate legLengths legDirs

  step (m, p) d = ((m', p'), v) where
    m' = (p, v) : m
    p' = (x+dx, y+dy)
    (dx,dy) = d
    (lx,ly) = turn d
    (x,y) = p
    v = sum . map get $ adjs
    adjs =
      [ (x+lx+dx, y+ly+dy)
      , (x+lx,    y+ly   )
      , (x+lx-dx, y+ly-dy)
      , (x   -dx, y   -dy)
      ]
    get at = case lookup at m of
      Just x  -> x
      Nothing -> 0

