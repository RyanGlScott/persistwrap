module PersistWrap.Table.Column where

import Data.Singletons (Sing, SingI (..))
import Data.Singletons.Prelude (SBool, SList, Snd)
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Structure (PrimName, SPrimName)

data Column = Column {name :: Symbol, nullability :: Bool, ctype :: PrimName}
data Schema = Schema {name :: Symbol, cols :: [Column]}

data instance Sing (col :: Column) where
  SColumn
    :: SSymbol name -> SBool nullability -> SPrimName ctype -> Sing ('Column name nullability ctype)
instance (SingI name, SingI nullability, SingI ctype)
    => SingI ('Column name nullability ctype) where
  sing = SColumn sing sing sing
data instance Sing (sch :: Schema) where
  SSchema :: SSymbol name -> SList cols -> Sing ('Schema name cols)
instance (SingI name, SingI cols) => SingI ('Schema name cols) where
  sing = SSchema sing sing
type family Cols (sch :: Schema) :: [Column] where
  Cols ('Schema name cols) = cols

type SColumn x = Sing (x :: Column)

type SchemaOf (tab :: (*,Schema)) = Snd tab
