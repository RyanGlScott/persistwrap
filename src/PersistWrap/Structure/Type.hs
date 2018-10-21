module PersistWrap.Structure.Type where

import Data.Promotion.Prelude (Symbol)
import Data.Singletons

import PersistWrap.Structure.Primitives

data Structure
  = Primitive PrimName
  | UnitType
  | EmptyType
  | SumType [(Symbol, Structure)]
  | ProductType [(Symbol, Structure)]
  | ListType Structure
  | MapType Structure -- Argument is value type; Key type is always Text

data instance Sing ('Primitive pn) where
  SPrimitive :: SPrimName pn -> Sing ('Primitive pn)
instance SingI n => SingI ('Primitive n) where
  sing = SPrimitive sing
data instance Sing 'EmptyType where
  SEmpty :: Sing 'EmptyType
instance SingI 'EmptyType where
  sing = SEmpty
data instance Sing 'UnitType where
  SUnit :: Sing 'UnitType
instance SingI 'UnitType where
  sing = SUnit
data instance Sing ('SumType xs) where
  SSumType :: Sing (xs :: [(Symbol, Structure)]) -> Sing ('SumType xs)
instance SingI xs => SingI ('SumType xs) where
  sing = SSumType sing
data instance Sing ('ProductType xs) where
  SProductType :: Sing (xs :: [(Symbol, Structure)]) -> Sing ('ProductType xs)
instance SingI xs => SingI ('ProductType xs) where
  sing = SProductType sing
data instance Sing ('ListType x) where
  SListType :: Sing s -> Sing ('ListType s)
instance SingI s => SingI ('ListType s) where
  sing = SListType sing
data instance Sing ('MapType x) where
  SMapType :: Sing s -> Sing ('MapType s)
instance SingI s => SingI ('MapType s) where
  sing = SMapType sing

type SStructure x = Sing (x :: Structure)
