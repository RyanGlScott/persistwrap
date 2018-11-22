{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PersistWrap.Table.Column where

import Data.Kind (type (*))
import Data.List.NonEmpty (NonEmpty)
import Data.Singletons.Prelude hiding (type (*))
import Data.Singletons.TH

import PersistWrap.Primitives (PrimName)

$(singletons [d|
  data BaseColumn text = Prim PrimName | Enum (NonEmpty text) | ForeignKey text | JSON
    deriving (Eq, Ord, Show)
  data Column text = Column Bool (BaseColumn text)
    deriving (Eq, Ord, Show)
  data Schema text = Schema text [(text,Column text)]
    deriving (Eq, Ord, Show)
  |])

$(singletonsOnly [d|
  schemaCols :: Schema Symbol -> [(Symbol, Column Symbol)]
  schemaCols (Schema _ cs) = cs
  schemaName :: Schema Symbol -> Symbol
  schemaName (Schema n _) = n
  |])

type TabSchema (tab :: (*,Schema Symbol)) = Snd tab
type TabName tab = SchemaName (TabSchema tab)
type TabCols tab = SchemaCols (TabSchema tab)
