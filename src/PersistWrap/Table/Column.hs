{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.Column where

import Data.Kind (type (*))
import Data.List.NonEmpty (NonEmpty)
import Data.Singletons.Prelude hiding (type (*))
import Data.Singletons.TH

import PersistWrap.Structure.Primitives (PrimName)

$(singletons [d|
  data BaseColumn text = Prim PrimName | Enum (NonEmpty text) | ForeignKey text | JSON
    deriving (Show)
  data Column text = Column Bool (BaseColumn text)
    deriving (Show)
  data Schema text = Schema text [(text,Column text)]
    deriving (Show)
  |])

$(singDecideInstances [''BaseColumn, ''Column, ''Schema])
$(singEqInstances [''BaseColumn, ''Column, ''Schema])
$(singOrdInstances [''BaseColumn, ''Column, ''Schema])

$(singletonsOnly [d|
  schemaCols :: Schema Symbol -> [(Symbol, Column Symbol)]
  schemaCols (Schema _ cs) = cs
  schemaName :: Schema Symbol -> Symbol
  schemaName (Schema n _) = n
  |])

type TabSchema (tab :: (*,Schema Symbol)) = Snd tab
type TabName tab = SchemaName (TabSchema tab)
type TabCols tab = SchemaCols (TabSchema tab)
