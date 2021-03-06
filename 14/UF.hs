{-# LANGUAGE BangPatterns #-}

module UF (UF, represent, insert, join, merge, size, UF.empty) where

import Data.Foldable (traverse_, foldr', fold)
import Data.Char (isDigit)
import Data.List (find, sortOn)
import Data.List.NonEmpty as NE (NonEmpty (..), head, nonEmpty, sort)
import Data.Semigroup ((<>))
import Data.Maybe (fromJust)
import Control.Applicative ((<|>), empty)

data UF a = UF [NonEmpty a]

size :: UF a -> Int
size (UF cs) = length cs

represent :: Eq a => UF a -> a -> Maybe a
represent (UF cs) a = foldr' (<|>) Nothing $ map check cs where
  check (rep :| es) = if (a == rep) || (a `elem` es) then Just rep else Nothing

insert :: Eq a => UF a -> a -> (UF a, a)
insert (UF !cs) !a = case break (elem a) cs of
  (!ls, !ac:rs) -> (UF $ ac : (ls ++ rs), NE.head ac)
  _             -> (UF $ (a :| []) : cs,  a         )

join :: Eq a => UF a -> a -> a -> UF a
join u !a !b = u' where
  (!ua,  !ra) = insert u  a
  (!uab, !rb) = insert ua b

  !u' = if ra == rb
    then uab
    else let
      (!ub'ac, !ac) = extract uab   ra
      (!u'abc, !bc) = extract ub'ac rb
      (UF !ocs) = u'abc
      in UF $ (ac <> bc) : ocs

  extract (UF !cs) rep = case break ((==rep) . NE.head) cs of
    (!ls, !c:rs) -> (UF $ ls ++ rs, c)
    _            -> error "oops"

merge :: Eq a => UF a -> UF a -> UF a
merge (UF []) v = v
merge u (UF !cs) = foldr' f u cs where
  f (!rep :| rest) !v = foldr' (\e w -> join w rep' e) v' rest where
    (v', rep') = insert v rep

empty :: UF a
empty = UF []

instance Eq a => Monoid (UF a) where
  mempty = UF.empty
  mappend = merge

