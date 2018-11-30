module Consin.Index
    ( IndexedItemS(..)
    , getWithIndexS
    ) where

import Data.Singletons
import Data.Singletons.Prelude (Sing(SCons, SNil))

import Conkin (Tagged(..))
import Conkin.Extra (Index(..), noHere)

data IndexedItemS xs f = forall x. SingI x => IndexedItemS (Index xs x) (f x)

getWithIndexS :: forall xs f . SingI xs => Tagged xs f -> IndexedItemS xs f
getWithIndexS = go (sing @_ @xs)
  where
    go :: forall xs' . Sing xs' -> Tagged xs' f -> IndexedItemS xs' f
    go = \case
      SNil -> noHere
      ((singInstance -> SingInstance) `SCons` sxs) -> \case
        Here  x                            -> IndexedItemS IHere x
        There (go sxs -> IndexedItemS i r) -> IndexedItemS (IThere i) r
