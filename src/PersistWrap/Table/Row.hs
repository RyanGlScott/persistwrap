module PersistWrap.Table.Row where

import Conkin (Tuple)

import PersistWrap.Structure (PrimType)
import PersistWrap.Table.Column

data Value (c :: Column) where
  V :: PrimType p -> Value ('Column sym 'False p)
  N :: Maybe (PrimType p) -> Value ('Column sym 'True p)

type Row xs = Tuple xs Value
