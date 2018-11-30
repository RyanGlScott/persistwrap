module Conkin.Extra.All
    ( All(..)
    , DictC(..)
    ) where

import Conkin (Tuple(..))
import Data.Constraint (Dict(Dict))

newtype DictC c x = DictC (Dict (c x))

class All c xs where
  dicts :: Tuple xs (DictC c)
instance All c '[] where
  dicts = Nil
instance (c x, All c xs) => All c (x ': xs) where
  dicts = DictC Dict `Cons` dicts
