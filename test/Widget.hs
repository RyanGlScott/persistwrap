{-# LANGUAGE UndecidableInstances #-}

module Widget where

import qualified Conkin
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics

import PersistWrap.Conkin.Extra.Class (AlwaysS, FromAlwaysS(FromAlwaysS))
import PersistWrap.Structure (EntityPart(..), GStructureOf, StructureOf)

data Color = Red | Green | Blue
  deriving (Eq, Show, Generic)
data Foo = Foo {bar :: Int64, baz :: Monster, qux :: Maybe Color}
  deriving (Eq, Show, Generic)
data Monster = A {x :: Int, y :: Int} | B | C | D | E Text
  deriving (Eq, Show, Generic)
data Widget fk
  = Foo1 Foo
  | Foo2 [Foo]
  | Blarg (Bool, Bool, ByteString)
  | Bleeble Text
  | Glorp (fk "abc")
  deriving (Generic)
instance Conkin.Functor Widget where
  fmap fn = \case
    Foo1 x -> Foo1 x
    Foo2 x -> Foo2 x
    Blarg x -> Blarg x
    Bleeble x -> Bleeble x
    Glorp x -> Glorp (fn x)
deriving instance {-# OVERLAPS #-} AlwaysS Eq fk => Eq (Widget (FromAlwaysS fk))
instance AlwaysS Eq fk => Eq (Widget fk) where
  (==) l r = Conkin.fmap FromAlwaysS l == Conkin.fmap FromAlwaysS r
deriving instance {-# OVERLAPS #-} AlwaysS Show fk => Show (Widget (FromAlwaysS fk))
instance AlwaysS Show fk => Show (Widget fk) where
  showsPrec d x = showsPrec d (Conkin.fmap FromAlwaysS x)

instance EntityPart fk Monster where
  type StructureOf Monster = GStructureOf (Rep Monster)
instance EntityPart fk Color where
  type StructureOf Color = GStructureOf (Rep Color)
instance EntityPart fk Foo where
  type StructureOf Foo = GStructureOf (Rep Foo)
instance EntityPart fk (Widget fk) where
  type StructureOf (Widget fk) = GStructureOf (Rep (Widget fk))
