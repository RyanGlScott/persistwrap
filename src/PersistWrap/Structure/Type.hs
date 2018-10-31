module PersistWrap.Structure.Type where

import Data.Promotion.Prelude (Symbol)
import Data.Singletons
import Data.Text (Text)

import PersistWrap.Structure.Primitives

data Structure
  = Primitive PrimName
  | UnitType
  | SumType (Symbol, Structure) [(Symbol, Structure)]
  | ProductType [(Symbol, Structure)]
  | ListType Structure
  | MapType PrimName Structure
data DStructure
  = DPrimitive PrimName
  | DUnitType
  | DSumType (Text, DStructure) [(Text, DStructure)]
  | DProductType [(Text, DStructure)]
  | DListType DStructure
  | DMapType PrimName DStructure

data instance Sing (struct :: Structure) where
  SPrimitive :: SPrimName pn -> Sing ('Primitive pn)
  SUnitType :: Sing 'UnitType
  SSumType
    :: Sing (x :: (Symbol, Structure)) -> Sing (xs :: [(Symbol, Structure)]) -> Sing ('SumType x xs)
  SProductType :: Sing (xs :: [(Symbol, Structure)]) -> Sing ('ProductType xs)
  SListType :: Sing s -> Sing ('ListType s)
  SMapType :: Sing k -> Sing v -> Sing ('MapType k v)
instance SingI n => SingI ('Primitive n) where
  sing = SPrimitive sing
instance SingI 'UnitType where
  sing = SUnitType
instance (SingI x, SingI xs) => SingI ('SumType x xs) where
  sing = SSumType sing sing
instance SingI xs => SingI ('ProductType xs) where
  sing = SProductType sing
instance SingI s => SingI ('ListType s) where
  sing = SListType sing
instance (SingI k, SingI v) => SingI ('MapType k v) where
  sing = SMapType sing sing
instance SingKind Structure where
  type Demote Structure = DStructure
  fromSing = \case
    SPrimitive pn -> DPrimitive (fromSing pn)
    SUnitType -> DUnitType
    SSumType xHead xTail -> DSumType (fromSing xHead) (fromSing xTail)
    SProductType xs -> DProductType (fromSing xs)
    SListType x -> DListType (fromSing x)
    SMapType k v -> DMapType (fromSing k) (fromSing v)
  toSing = \case
    DPrimitive n -> case toSing n of
      SomeSing sn -> SomeSing $ SPrimitive sn
    DUnitType -> SomeSing SUnitType
    DSumType xHead xTail -> case (toSing xHead, toSing xTail) of
      (SomeSing sxHead, SomeSing sxTail) -> SomeSing $ SSumType sxHead sxTail
    DProductType xs -> case toSing xs of
      SomeSing sxs -> SomeSing $ SProductType sxs
    DListType x -> case toSing x of
      SomeSing sx -> SomeSing $ SListType sx
    DMapType k v -> case (toSing k, toSing v) of
      (SomeSing sk, SomeSing sv) -> SomeSing $ SMapType sk sv

type SStructure x = Sing (x :: Structure)
