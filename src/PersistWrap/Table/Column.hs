{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.Column where

import Data.Singletons (Sing, SingI (..))
import Data.Singletons.Prelude (SBool, SList, Snd)
import Data.Singletons.TH (singletonsOnly, singDecideInstances)
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Structure (PrimName, SPrimName)

data BaseColumn where
  Prim :: PrimName -> BaseColumn
  ForeignKey :: Symbol -> BaseColumn
  JSON :: BaseColumn
data Column = Column {name :: Symbol, nullability :: Bool, ctype :: BaseColumn}
data Schema = Schema {name :: Symbol, cols :: [Column]}

data instance Sing (bc :: BaseColumn) where
  SPrim :: SPrimName pn -> Sing ('Prim pn)
  SForeignKey :: SSymbol n -> Sing ('ForeignKey n)
  SJSON :: Sing 'JSON
instance SingI pn => SingI ('Prim pn) where
  sing = SPrim sing
instance SingI n => SingI ('ForeignKey n) where
  sing = SForeignKey sing
instance SingI 'JSON where
  sing = SJSON
type SBaseColumn x = Sing (x :: BaseColumn)

data instance Sing (x :: Column) where
  SColumn
    :: SSymbol name
    -> SBool nullability
    -> SBaseColumn ctype
    -> Sing ('Column name nullability ctype)
instance (SingI name, SingI nullability, SingI ctype)
    => SingI ('Column name nullability ctype) where
  sing = SColumn sing sing sing
type SColumn x = Sing (x :: Column)

data instance Sing (sch :: Schema) where
  SSchema :: SSymbol name -> SList cols -> Sing ('Schema name cols)
instance (SingI name, SingI cols) => SingI ('Schema name cols) where
  sing = SSchema sing sing
type SSchema x = Sing (x :: Schema)

$(singDecideInstances [''BaseColumn, ''Column, ''Schema])

$(singletonsOnly [d|
  schemaCols :: Schema -> [Column]
  schemaCols (Schema _ cs) = cs
  schemaName :: Schema -> Symbol
  schemaName (Schema n _) = n
  |])

type TabSchema (tab :: (*,Schema)) = Snd tab
type TabName tab = SchemaName (TabSchema tab)
type TabCols tab = SchemaCols (TabSchema tab)
