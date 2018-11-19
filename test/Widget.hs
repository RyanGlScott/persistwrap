{-# LANGUAGE UndecidableInstances #-}

module Widget where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics

import PersistWrap.Structure.EntityPart (EntityPart(..), GStructureOf)

data Color = Red | Green | Blue
  deriving Generic
data Foo = Foo {bar :: Int64, baz :: Monster, qux :: Maybe Color}
  deriving Generic
data Widget fk
    = Foo1 Foo | Foo2 [Foo] | Blarg (Bool, Bool, ByteString) | Bleeble Text | Glorp (fk "abc")
  deriving Generic
data Monster = A {x :: Int, y :: Int} | B | C | D | E Text
  deriving Generic

instance EntityPart fk Monster where
  type StructureOf Monster = GStructureOf (Rep Monster)
instance EntityPart fk Color where
  type StructureOf Color = GStructureOf (Rep Color)
instance EntityPart fk Foo where
  type StructureOf Foo = GStructureOf (Rep Foo)
instance EntityPart fk (Widget fk) where
  type StructureOf (Widget fk) = GStructureOf (Rep (Widget fk))
