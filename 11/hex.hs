{-# LANGUAGE LambdaCase, StandaloneDeriving #-}

import Data.Char (toUpper)
import Control.Applicative (liftA2)
import Data.Foldable (foldr')

data Dir = N | S | NE | NW | SE | SW deriving (Read, Show)

data V2 a = V2 !a !a

deriving instance Show a => Show (V2 a)

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)

instance Applicative V2 where
  pure x = V2 x x
  (V2 f g) <*> (V2 x y) = V2 (f x) (g y)

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

(*^) :: Num a => a -> V2 a -> V2 a
k *^ (V2 x y) = V2 (k*x) (k*y)

instance Foldable V2 where
  foldMap f (V2 x y) = f x `mappend` f y

toVec :: Dir -> V2 Int
toVec = \case
  N  -> V2 ( 0) ( 2)
  S  -> V2 ( 0) (-2)
  NE -> V2 ( 1) ( 1)
  NW -> V2 (-1) ( 1)
  SE -> V2 ( 1) (-1)
  SW -> V2 (-1) (-1)

nSteps disp = diag + ortho where
  diag = minimum (abs disp)
  ortho = case abs (disp - diag *^ signum disp) of
    V2 x y | y <= 1 -> x
    V2 0 y          -> y `div` 2

parse :: String -> [Dir]
parse "" = []
parse s = read (map toUpper pref) : parse (drop 1 suff) where
  (pref, suff) = break (==',') s

main = do
  input <- parse <$> readFile "input"
--let input = [SE,NE,N,N]
  let disp = foldr' (+) 0 $ map toVec input
  putStrLn.show $ disp
  putStrLn.show $ nSteps disp

