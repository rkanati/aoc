
main :: IO ()
main = do
  input <- readFile "input"
  let table = map (map read . words) . lines $ input
  putStrLn.show.checksum $ table

checksum :: [[Integer]] -> Integer
checksum = sum . map rowCheck where
  rowCheck row = maximum row - minimum row

