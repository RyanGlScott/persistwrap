{-# LANGUAGE UndecidableInstances #-}

module Widget where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics

import PersistWrap.Conkin.Extra.Class (Always, (==*), showsPrec1)
import PersistWrap.Structure (EntityPart(..), GStructureOf, StructureOf)

data Color = Red | Green | Blue
  deriving (Eq, Show, Generic)
data Foo = Foo {bar :: Int64, baz :: Monster, qux :: Maybe Color}
  deriving (Eq, Show, Generic)
data Widget fk
  = Foo1 Foo
  | Foo2 [Foo]
  | Blarg (Bool, Bool, ByteString)
  | Bleeble Text
  | Glorp (Foreignated fk)
  deriving (Generic)
deriving instance Always Eq fk => Eq (Widget fk)
deriving instance Always Show fk => Show (Widget fk)
data Monster = A {x :: Int, y :: Int} | B | C | D | E Text
  deriving (Eq, Show, Generic)

newtype Foreignated fk = Foreignated (fk "abc")
  deriving (Generic)
instance Always Eq fk => Eq (Foreignated fk) where
  (==) (Foreignated l) (Foreignated r) = l ==* r
instance Always Show fk => Show (Foreignated fk) where
  showsPrec d (Foreignated x) = showsPrec1 d x

instance EntityPart fk Monster where
  type StructureOf Monster = GStructureOf (Rep Monster)
instance EntityPart fk Color where
  type StructureOf Color = GStructureOf (Rep Color)
instance EntityPart fk Foo where
  type StructureOf Foo = GStructureOf (Rep Foo)
instance EntityPart fk (Foreignated fk) where
  type StructureOf (Foreignated fk) = GStructureOf (Rep (Foreignated fk))
instance EntityPart fk (Widget fk) where
  type StructureOf (Widget fk) = GStructureOf (Rep (Widget fk))
