{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.Column where

import Data.List.NonEmpty (NonEmpty)
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits (Symbol)

import PersistWrap.Structure.Primitives (PrimName)

$(singletons [d|
  data BaseColumn text = Prim PrimName | Enum (NonEmpty text) | ForeignKey text | JSON
  data Column text = Column Bool (BaseColumn text)
  data Schema text = Schema text [(text,Column text)]
  |])

deriving instance Show text => Show (BaseColumn text)
deriving instance Show text => Show (Column text)
deriving instance Show text => Show (Schema text)

$(singDecideInstances [''BaseColumn, ''Column, ''Schema])
$(singEqInstances [''BaseColumn, ''Column, ''Schema])
$(singOrdInstances [''BaseColumn, ''Column, ''Schema])
$(singShowInstances [''BaseColumn, ''Column, ''Schema])

$(singletonsOnly [d|
  schemaCols :: Schema Symbol -> [(Symbol, Column Symbol)]
  schemaCols (Schema _ cs) = cs
  schemaName :: Schema Symbol -> Symbol
  schemaName (Schema n _) = n
  |])

type TabSchema (tab :: (*,Schema Symbol)) = Snd tab
type TabName tab = SchemaName (TabSchema tab)
type TabCols tab = SchemaCols (TabSchema tab)
