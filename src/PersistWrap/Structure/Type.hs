module PersistWrap.Structure.Type where

import Data.Promotion.Prelude (Symbol)
import Data.Singletons

import PersistWrap.Structure.Primitives

data Structure
  = Primitive PrimName
  | UnitType
  | SumType [(Symbol, Structure)]
  | ProductType [(Symbol, Structure)]
  | ListType Structure
  | MapType Structure -- Argument is value type; Key type is always Text

data instance Sing (struct :: Structure) where
  SPrimitive :: SPrimName pn -> Sing ('Primitive pn)
  SUnitType :: Sing 'UnitType
  SSumType :: Sing (xs :: [(Symbol, Structure)]) -> Sing ('SumType xs)
  SProductType :: Sing (xs :: [(Symbol, Structure)]) -> Sing ('ProductType xs)
  SListType :: Sing s -> Sing ('ListType s)
  SMapType :: Sing s -> Sing ('MapType s)
instance SingI n => SingI ('Primitive n) where
  sing = SPrimitive sing
instance SingI 'UnitType where
  sing = SUnitType
instance SingI xs => SingI ('SumType xs) where
  sing = SSumType sing
instance SingI xs => SingI ('ProductType xs) where
  sing = SProductType sing
instance SingI s => SingI ('ListType s) where
  sing = SListType sing
instance SingI s => SingI ('MapType s) where
  sing = SMapType sing

type SStructure x = Sing (x :: Structure)
