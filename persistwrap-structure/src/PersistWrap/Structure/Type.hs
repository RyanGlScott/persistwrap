{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.Type where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Singletons.TH (singletons)

import PersistWrap.Primitives

$(singletons [d|
  data Structure text
    = Primitive PrimName
    | Foreign text
    | UnitType
    | SumType (NonEmpty (text, Structure text))
    | ProductType [(text, Structure text)]
    | ListType (Structure text)
    | MapType (Structure text) (Structure text)
  |])

data StructTag =
  PrimitiveC | ForeignC | UnitTypeC | SumTypeC | ProductTypeC | ListTypeC | MapTypeC
  deriving (Bounded,Enum)

_constructorOf :: Structure text -> StructTag
_constructorOf = \case
  Primitive{}   -> PrimitiveC
  Foreign{}     -> ForeignC
  UnitType{}    -> UnitTypeC
  SumType{}     -> SumTypeC
  ProductType{} -> ProductTypeC
  ListType{}    -> ListTypeC
  MapType{}     -> MapTypeC

terminals :: [StructTag]
terminals = [PrimitiveC, ForeignC, UnitTypeC, ProductTypeC]
