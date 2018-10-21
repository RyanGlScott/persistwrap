module PersistWrap.Table.Column where

import Data.Singletons (Sing, SingI (..))
import Data.Singletons.Prelude (SBool, Snd)
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Structure (PrimName, SPrimName)

data Column = Column {name :: Symbol, nullability :: Bool, ctype :: PrimName}

data instance Sing ('Column name nullability ctype) where
  SColumn
    :: SSymbol name -> SBool nullability -> SPrimName ctype -> Sing ('Column name nullability ctype)
instance (SingI name, SingI nullability, SingI ctype)
    => SingI ('Column name nullability ctype) where
  sing = SColumn sing sing sing

type SColumn = Sing Column

type Schema (tab :: (*,[Column])) = Snd tab
