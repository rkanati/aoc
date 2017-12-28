
import Data.List (splitAt, foldl', elemIndex)
import Data.Char (isDigit)

data Move
  = Spin !Int
  | Exchange !Int !Int
  | Partner !Char !Char

dance :: String -> [Move] -> String
dance = foldl' makeMove where
  makeMove s (Spin       n) = sx ++ px where (px, sx) = splitAt (length s - n) s
  makeMove s (Exchange i j) | i /= j = px ++ (y:mx) ++ (x:sx) where
    i' = min i j; j' = max i j
    (px, x:mxysx) = splitAt i' s
    (mx, y:sx   ) = splitAt (j'-i'-1) mxysx
  makeMove s (Partner  a b) | a /= b = makeMove s (Exchange i j) where
    (Just i) = elemIndex a s; (Just j) = elemIndex b s
  makeMove s _ = s

parse :: String -> [Move]
parse "" = []
parse ('s':cs) = Spin (read ncs) : parse rest where
  (ncs, rest) = span isDigit cs
parse ('x':cs) = Exchange (read ics) (read jcs) : parse rest where
  (ics, _:cs') = span isDigit cs
  (jcs,  rest) = span isDigit cs'
parse ('p':a:_:b:cs) = Partner a b : parse cs
parse (c:cs) = parse cs

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  let final = dance "abcdefghijklmnop" input
  putStrLn final

