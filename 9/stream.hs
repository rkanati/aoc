{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}

import Control.Applicative
import Control.Arrow (first)

data Item
  = Group [Item]
  | Garbage !Int
  deriving (Show)

scoreStream :: Item -> Int
scoreStream = go 1 where
  go lv (Group cs) = lv + sum (map (go (lv + 1)) cs)
  go _  (Garbage _) = 0

weighGarbage :: Item -> Int
weighGarbage (Group cs) = sum (map weighGarbage cs)
weighGarbage (Garbage w) = w

data P a = P { runParser :: String -> Maybe (a, String) }

instance Functor P where
  fmap f p = P $ fmap (first f) . runParser p

instance Applicative P where
  pure x = P $ \cs -> pure (x, cs)
  pf <*> px = P $ \cs -> do
    (f, cs' ) <- runParser pf cs
    runParser (fmap f px) cs'

instance Alternative P where
  empty = P $ \_ -> Nothing
  pa <|> pb = P $ \cs ->
    runParser pa cs <|> runParser pb cs

instance Monad P where
  p >>= f = P $ \cs -> do
    (x, cs') <- runParser p cs
    runParser (f x) cs'

expect :: Char -> P Char
expect e = P $ \case
  (c:cs) -> if c == e then Just (c, cs) else Nothing
  _      -> Nothing

get :: P Char
get = P $ \case
  (c:cs) -> Just (c, cs)
  _      -> Nothing

list :: Char -> P a -> P [a]
list sep p = ((:) <$> p <*> many (expect sep *> p)) <|> pure []

parseGarbage :: P Item
parseGarbage = (Garbage . sum)
  <$  expect '<'
  <*> many (get >>= \case
        '!' -> 0 <$ get
        '>' -> empty
        _   -> pure 1)
  <*  expect '>'

parseItem :: P Item
parseItem = parseGarbage <|> parseGroup

parseGroup :: P Item
parseGroup = Group
  <$  expect '{'
  <*> list ',' parseItem
  <*  expect '}'

evalParser :: P a -> String -> Maybe a
evalParser p = fmap fst . runParser p

main :: IO ()
main = do
  input <- readFile "input"
  let (Just root)     = evalParser parseGroup input
  putStrLn.show.scoreStream $ root
  putStrLn.show.weighGarbage $ root

