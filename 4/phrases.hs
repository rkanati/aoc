
import Data.List (nub, nubBy, sort)

main :: IO ()
main = do
  phrases <- (map words . lines) <$> readFile "input"
  putStrLn.show.length.filter noDups $ phrases
  putStrLn.show.length.filter noAnas $ phrases

  where
  noDups ws = length ws == length (nub ws)
  noAnas ws = length ws == length (nubBy isAnagram ws)
  isAnagram a b = sort a == sort b

