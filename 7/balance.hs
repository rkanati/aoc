
import Data.Char (isAlpha)
import Data.List (mapAccumL, intercalate)
import Data.Either (partitionEithers)

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

findImbalance :: Node -> Int
findImbalance (Node _ w cs)

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

