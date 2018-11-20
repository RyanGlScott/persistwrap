{-# LANGUAGE UndecidableInstances #-}

module Widget where

import qualified Conkin
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics

import PersistWrap.Conkin.Extra.Class (Always, FromAlways(FromAlways))
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
deriving instance {-# OVERLAPS #-} Always Eq fk => Eq (Widget (FromAlways fk))
instance Always Eq fk => Eq (Widget fk) where
  (==) l r = Conkin.fmap FromAlways l == Conkin.fmap FromAlways r
deriving instance {-# OVERLAPS #-} Always Show fk => Show (Widget (FromAlways fk))
instance Always Show fk => Show (Widget fk) where
  showsPrec d x = showsPrec d (Conkin.fmap FromAlways x)

instance EntityPart fk Monster where
  type StructureOf Monster = GStructureOf (Rep Monster)
instance EntityPart fk Color where
  type StructureOf Color = GStructureOf (Rep Color)
instance EntityPart fk Foo where
  type StructureOf Foo = GStructureOf (Rep Foo)
instance EntityPart fk (Widget fk) where
  type StructureOf (Widget fk) = GStructureOf (Rep (Widget fk))
