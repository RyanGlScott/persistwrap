module Conkin.Extra.Index
    ( Index(..)
    , IndexedItem(..)
    , getWithIndex
    ) where

import Conkin (Tagged(..))

data Index xs x where
  IHere :: Index (x ': xs) x
  IThere :: Index xs x -> Index (x0 ': xs) x

data IndexedItem xs f = forall x. IndexedItem (Index xs x) (f x)

getWithIndex :: Tagged xs f -> IndexedItem xs f
getWithIndex = \case
  Here  x -> IndexedItem IHere x
  There (getWithIndex -> IndexedItem i r) -> IndexedItem (IThere i) r
