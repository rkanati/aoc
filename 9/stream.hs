{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}

import Control.Applicative
import Control.Arrow (first)
import Data.Maybe (catMaybes)

data Group = Group [Group] deriving (Show)

scoreStream :: Group -> Int
scoreStream = go 1 where
  go lv (Group cs) = lv + sum (map (go (lv + 1)) cs)

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

parseGarbage :: P ()
parseGarbage = ()
  <$  expect '<'
  <*  many (get >>= \case
        '!' -> () <$ get
        '>' -> empty
        _   -> pure ())
  <*  expect '>'

parseItem :: P (Maybe Group)
parseItem = (Nothing <$ parseGarbage) <|> (Just <$> parseGroup)

parseGroup :: P Group
parseGroup = Group
  <$  expect '{'
  <*> (catMaybes <$> list ',' parseItem)
  <*  expect '}'

--parseText :: P String
--parseText = many $ (parseGarbage *> get) <|> get

evalParser :: P a -> String -> Maybe a
evalParser p = fmap fst . runParser p

main :: IO ()
main = do
  input <- readFile "input"
--let input = "{<!>!!}{{}}{}{}{}}!<!!!<>{},{}}"
--let (Just stripped) = evalParser parseText input
  let (Just root)     = evalParser parseGroup input
--putStrLn.show $ root
  putStrLn.show.scoreStream $ root

