
import Data.Char (isAlpha)
import Data.List (mapAccumL, intercalate)
import Data.Either (partitionEithers)
import Data.Maybe (fromJust)
import Data.Foldable (foldr')
import Data.Monoid (Sum(..))

data Node a = Node String a [Node a]

data RefNode = RefNode String Int [String]

indent :: [Bool] -> String
indent lvs = go True lvs where
  go _ []             = ""
  go deepest (notLast:lvs') = go False lvs' ++ cs where
    cs = case (notLast, deepest) of
      (True,  True ) -> "├"
      (True,  False) -> "│ "
      (False, True ) -> "└"
      (False, False) -> "  "

showNode :: Show a => [Bool] -> Node a -> String
showNode is (Node n w cs) = indent is ++ n ++ " " ++ show w
  ++ intercalate "\n" ("":showKids cs)
  where showKids []     = []
        showKids [c]    = [showNode (False:is) c]
        showKids (c:cs) = showNode (True:is) c : showKids cs

instance Show a => Show (Node a) where
  show = showNode []

argMax f xs = fmap fst $ foldr' choose Nothing $ zip xs (map f xs) where
  choose b          Nothing          = Just b
  choose b@(_, fx') (Just a@(_, fx)) = Just $ if fx' > fx then b else a

findImbalance :: Node Int -> Int
findImbalance = go 0 . weighTree where
  go target (Node _ (w, w') cs) = dw where
    dw =
      if all (== maxw) cws
        then w + err
        else if err > 0
          then go maxw (fromJust $ argMax (negate.getWeight) cs)
          else go minw (fromJust $ argMax         getWeight  cs)
    err = target - w'
    cws = map getWeight cs
    minw = minimum cws
    maxw = maximum cws

  weighTree (Node n w cs) = Node n (w, w') cs' where
    cs' = map weighTree cs
    w' = w + getSum (foldMap (Sum . getWeight) cs')

  getWeight      (Node _ (_, w) _) = w
  getTowerWeight (Node _ (w, _) _) = w

buildTree :: [Node Int] -> [RefNode] -> Node Int
buildTree [n] [] = n
buildTree ns (RefNode n w cs : rs) =
  case sequence mcs' of
    Just cs' -> buildTree (Node n w cs' : ns') rs
    _        -> buildTree ns (rs ++ [RefNode n w cs])
  where
  (ns', mcs') = mapAccumL getChild ns cs
  getChild ns c =
    case break pred ns of
      (ls, n:rs) -> (ls++rs, Just n)
      (ls, [])   -> (ls,     Nothing)
    where
    pred (Node s _ _) = s == c

parseNode :: [String] -> Either (Node Int) RefNode
parseNode (n:w:_:cs) = Right $ RefNode n (read w) (map uncomma cs) where
  uncomma = filter isAlpha
parseNode (n:w:[]) = Left $ Node n (read w) []

main :: IO ()
main = do
  table <- (map (parseNode.words).lines) <$> readFile "input"
  let (leaves, refs) = partitionEithers table
  let root = buildTree leaves refs
  putStrLn $ show root
  putStrLn $ show (findImbalance root)

