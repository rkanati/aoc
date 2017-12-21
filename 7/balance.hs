
import Data.Char (isAlpha)
import Data.List (mapAccumL, intercalate)
import Data.Either (partitionEithers)
import Data.Maybe (fromJust)

data Node = Node String Int [Node]

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

showNode :: [Bool] -> Node -> String
showNode is (Node n w cs) = indent is ++ n ++ " " ++ show w
  ++ intercalate "\n" ("":showKids cs)
  where showKids []     = []
        showKids [c]    = [showNode (False:is) c]
        showKids (c:cs) = showNode (True:is) c : showKids cs

instance Show Node where
  show = showNode []

argmax f xs = fmap fst $ foldr' choose Nothing $ zip xs (map f xs) where
  choose Nothing        (x', fx') = Just (x', fx')
  choose (Just (x, fx)) (x', fx') =
    Just $ if fx' > fx then (x', fx') else (x, fx)

findImbalance :: Node -> Int
findImbalance = go 0 . weighTree where
  go target (Node _ w []) = target - w
  go target (Node _ w cs) =
    case argMax (abs.go target') cs of
      Just dw -> dw
      Nothing -> 
    target' = (target - w) `div` (length cs)

  weighTree (Node n w cs) = Node n w' cs' where
    cs' = map weighTree cs
    w' = w + foldMap getWeight cs'

  getWeight (Node _ w _) = w

buildTree :: [Node] -> [RefNode] -> Node
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

parseNode :: [String] -> Either Node RefNode
parseNode (n:w:_:cs) = Right $ RefNode n (read w) (map uncomma cs) where
  uncomma = filter isAlpha
parseNode (n:w:[]) = Left $ Node n (read w) []

main :: IO ()
main = do
  table <- (map (parseNode.words).lines) <$> readFile "input"
  let (leaves, refs) = partitionEithers table
  let root = buildTree leaves refs
  putStrLn $ show root

