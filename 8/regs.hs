import Data.Foldable (traverse_)
import Data.List (mapAccumL)

data Compare = Gt | GE | Lt | LE | Eq | NE deriving (Show)

parseComp ('>':'=':_) = GE
parseComp ('<':'=':_) = LE
parseComp ('>':_)     = Gt
parseComp ('<':_)     = Lt
parseComp ('=':'=':_) = Eq
parseComp ('!':'=':_) = NE

doCompare GE = (>=)
doCompare LE = (<=)
doCompare Gt = (>)
doCompare Lt = (<)
doCompare Eq = (==)
doCompare NE = (/=)

data Test = Test Compare String Int deriving (Show)

parseTest :: [String] -> Test
parseTest ("if":reg:cmp:imm:[]) = Test (parseComp cmp) reg (read imm)

data Instruction = Inst Int String Test deriving (Show)

parseInstr :: [String] -> Instruction
parseInstr (reg:op:imm:test) = Inst d reg $ parseTest test where
  d = case op of
    "inc" ->          read imm
    "dec" -> negate $ read imm

run :: [Instruction] -> [(String, Int)]
run = fst . mapAccumL go [] where
  go regs (Inst delta dest test) =
    if doTest regs test
      then (incReg regs dest delta, ())
      else (regs, ())
  doTest regs (Test cmp reg ref) = doCompare cmp val ref where
    val = maybe 0 id $ lookup reg regs
  incReg regs reg delta =
    case break (\(k, _) -> k == reg) regs of
      (ls, (_, v) : rs) -> (reg, v + delta) : (ls ++ rs)
      (_,  [])          -> (reg,     delta) : regs

main :: IO ()
main = do
  input <- (map (parseInstr.words) . lines) <$> readFile "input"
  let regs = run input
  putStrLn.show.maximum.map snd $ regs
--traverse_ (putStrLn.show) input

