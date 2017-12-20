
import Data.List (nub)

main :: IO ()
main = putStrLn.show.length.filter valid =<< phrases where
  phrases = (map words . lines) <$> readFile "input"
  valid ws = length ws == length (nub ws)

