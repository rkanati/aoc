
import Data.Char (isDigit)
import Data.Foldable (traverse_, foldr', for_)
import Data.List (tails, elemIndex, uncons, find)
import Control.Monad (guard, when, mzero)
import Data.Maybe (catMaybes, isJust)

data Scanner
  = Scanner { depth :: !Int, limit :: !Int, pos :: !Int, dir :: !Int }
  deriving (Show, Eq)

type Firewall = [Scanner]

parse :: String -> Scanner
parse s = Scanner (read sd) (read sr - 1) 0 1 where
  (sd, rest) = span isDigit s
  sr = dropWhile (not.isDigit) rest

advance :: Scanner -> Scanner
advance s = s { pos = pos', dir = dir' } where
  !pos' = pos s + dir s
  !dir' = if pos' == limit s || pos' == 0
    then -(dir s)
    else  (dir s)

caught :: Scanner -> Bool
caught s = pos s == 0

severity :: Scanner -> Int
severity s = if caught s then (limit s + 1) * depth s else 0

period :: Scanner -> Int
period s = 2 * limit s

scoreFrom :: Int -> [Firewall] -> Maybe Int
scoreFrom n states
  = fmap (\ss -> let !t = sum ss in t)
  . (\ss -> if null ss then Nothing else Just ss)
  . catMaybes
  . map (>>= \s -> if caught s then Just (severity s) else Nothing)
  . take n
  . zipWith findScanner [0..]
  $ states
  where
  findScanner d = find ((==d).depth)

main :: IO ()
main = do
  w <- map parse . lines <$> readFile "input"
--let w = map parse [ "0: 3", "1: 2", "4: 4", "6: 4" ]
  let p0s = map depth w
      periods = map period w
      pstates = iterate (map (+1)) p0s
    --(Just start) = elemIndex (all (==0)) pstates

  for_ (zip [0..] pstates) $ \(i, pstate) -> do
    when (i `mod` 50000 == 0) (putStrLn $ show i)
    let stop = all (/=0) $ zipWith mod pstate periods
    when stop $ do
      putStrLn.show $ i
      mzero

--putStrLn.show $ start
    --n = maximum (map depth w) + 1
    --states = iterate (map advance) w
    --scores = map (scoreFrom n) (tails states)
    --scores' = zip [0..] scores
    --test (i, _) = all (/=0) $ map (i `mod`) periods
    --pruned = filter test scores'

--for_ pruned $ \(i, s) -> do
--  putStrLn $ show i ++ ": " ++ show s
--  guard $ isJust s

