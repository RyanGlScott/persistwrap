{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.Column where

import Data.Singletons (Sing, SingI(..))
import Data.Singletons.Prelude
import Data.Singletons.TH (singDecideInstances, singEqInstances, singOrdInstances, singletonsOnly)
import Data.Singletons.TypeLits (SSymbol, Symbol)
import Data.Text (Text)

import PersistWrap.Structure (PrimName, SPrimName)

data BaseColumn = Prim PrimName | Enum Symbol [Symbol] | ForeignKey Symbol | JSON
data DBaseColumn = DPrim PrimName | DEnum Text [Text] | DForeignKey Text | DJSON
data Column = Column Bool BaseColumn
data DColumn = DColumn Bool DBaseColumn
data Schema = Schema Symbol [(Symbol,Column)]
data DSchema = DSchema Text [(Text,DColumn)]

data instance Sing (bc :: BaseColumn) where
  SPrim :: SPrimName pn -> Sing ('Prim pn)
  SEnum :: SSymbol x -> SList xs -> Sing ('Enum x xs)
  SForeignKey :: SSymbol n -> Sing ('ForeignKey n)
  SJSON :: Sing 'JSON
instance SingI pn => SingI ('Prim pn) where
  sing = SPrim sing
instance (SingI x, SingI xs) => SingI ('Enum x xs) where
  sing = SEnum sing sing
instance SingI n => SingI ('ForeignKey n) where
  sing = SForeignKey sing
instance SingI 'JSON where
  sing = SJSON
type SBaseColumn x = Sing (x :: BaseColumn)
instance SingKind BaseColumn where
  type Demote BaseColumn = DBaseColumn
  fromSing = \case
    SPrim pn -> DPrim $ fromSing pn
    SEnum x xs -> DEnum (fromSing x) (fromSing xs)
    SForeignKey n -> DForeignKey (fromSing n)
    SJSON -> DJSON
  toSing = \case
    DPrim pn -> case toSing pn of
      SomeSing spn -> SomeSing $ SPrim spn
    DEnum x xs -> case (toSing x, toSing xs) of
      (SomeSing sx, SomeSing sxs) -> SomeSing $ SEnum sx sxs
    DForeignKey n -> case toSing n of
      SomeSing sn -> SomeSing $ SForeignKey sn
    DJSON -> SomeSing SJSON

data instance Sing (x :: Column) where
  SColumn
    :: SBool nullability
    -> SBaseColumn ctype
    -> Sing ('Column nullability ctype)
instance (SingI nullability, SingI ctype) => SingI ('Column nullability ctype) where
  sing = SColumn sing sing
type SColumn x = Sing (x :: Column)
instance SingKind Column where
  type Demote Column = DColumn
  fromSing (SColumn n bc) = DColumn (fromSing n) (fromSing bc)
  toSing (DColumn n bc) = case (toSing n, toSing bc) of
    (SomeSing sn, SomeSing sbc) -> SomeSing $ SColumn sn sbc

data instance Sing (sch :: Schema) where
  SSchema :: SSymbol name -> SList cols -> Sing ('Schema name cols)
instance (SingI name, SingI cols) => SingI ('Schema name cols) where
  sing = SSchema sing sing
type SSchema x = Sing (x :: Schema)
instance SingKind Schema where
  type Demote Schema = DSchema
  fromSing (SSchema n cols) = DSchema (fromSing n) (fromSing cols)
  toSing (DSchema n cols) = case (toSing n, toSing cols) of
    (SomeSing sn, SomeSing scols) -> SomeSing $ SSchema sn scols

$(singDecideInstances [''BaseColumn, ''Column, ''Schema])
$(singEqInstances [''BaseColumn, ''Column, ''Schema])
$(singOrdInstances [''BaseColumn, ''Column, ''Schema])

$(singletonsOnly [d|
  schemaCols :: Schema -> [(Symbol, Column)]
  schemaCols (Schema _ cs) = cs
  schemaName :: Schema -> Symbol
  schemaName (Schema n _) = n
  |])

type TabSchema (tab :: (*,Schema)) = Snd tab
type TabName tab = SchemaName (TabSchema tab)
type TabCols tab = SchemaCols (TabSchema tab)
