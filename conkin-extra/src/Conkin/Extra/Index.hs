module Conkin.Extra.Index
    ( Index(..)
    , IndexedItem(..)
    , getWithIndex
    , tupleLens
    , uncheckIndex
    ) where

import Conkin (Tagged(..), Tuple(..))

data Index xs x where
  IHere :: Index (x ': xs) x
  IThere :: Index xs x -> Index (x0 ': xs) x

data IndexedItem xs f = forall x. IndexedItem (Index xs x) (f x)

getWithIndex :: Tagged xs f -> IndexedItem xs f
getWithIndex = \case
  Here  x -> IndexedItem IHere x
  There (getWithIndex -> IndexedItem i r) -> IndexedItem (IThere i) r

uncheckIndex :: Index xs x -> Int
uncheckIndex = \case
  IHere -> 0
  IThere i -> uncheckIndex i + 1

tupleLens :: Functor func => Index xs x -> (f x -> func (f x)) -> Tuple xs f -> func (Tuple xs f)
tupleLens IHere fn (x `Cons` xs) = (`Cons` xs) <$> fn x
tupleLens (IThere i) fn (x `Cons` xs) = (x `Cons`) <$> tupleLens i fn xs
