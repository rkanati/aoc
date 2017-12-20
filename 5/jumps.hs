
import Data.List (unfoldr)

main :: IO ()
main = do
  input <- (map read.lines) <$> readFile "input"
  --putStrLn.show.solve (+1) $ input
  --let input = [ 0, 3, 0, 1, -3 ]
  let adj2 j = if j >= 3 then (j-1) else (j+1)
  putStrLn.show.solve adj2 $ input

solve :: (Int -> Int) -> [Int] -> Int
solve adj (j:js) = 1 + length (unfoldr step ([], j, js)) where
  step (ps, j, ns)
    | j == 0 = Just ((), (ps, adj j, ns))
    |  j > length ns = Nothing
    | -j > length ps = Nothing
    | j >  0 = let (overs,j':ns') = splitAt (j - 1) ns
                   ps' = ps ++ [adj j] ++ overs
                in Just ((), (ps', j', ns'))
    | j <  0 = let (ps',j':overs) = splitAt (length ps + j) ps
                   ns' = overs ++ [adj j] ++ ns
                in Just ((), (ps', j', ns'))

