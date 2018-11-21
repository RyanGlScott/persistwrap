{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.SpecUtil.Widget where

import qualified Conkin
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Generics.Deriving.Eq (GEq(..))
import Generics.Deriving.Show (GShow(..))
import GHC.Generics (Generic, Rep)

import PersistWrap.Conkin.Extra.TH (deriveFnEq, deriveFnShow)
import PersistWrap.SpecUtil.Orphans ()
import PersistWrap.Structure (EntityPart, StructureOf, GStructureOf)
import PersistWrap.Structure.TH (deriveEntityPart)

data Color = Red | Green | Blue
  deriving (Eq, Show, Generic)
$(deriveEntityPart [t| Color |])

data Monster = A {x :: Int, y :: Int} | B | C | D | E Text
  deriving (Eq, Show, Generic)
$(deriveEntityPart [t| Monster |])

data Foo = Foo {bar :: Int64, baz :: Monster, qux :: Maybe Color}
  deriving (Eq, Show, Generic)
$(deriveEntityPart [t| Foo |])

-- |
-- `fk` is the type of a foreign key. Here we're declaring that the `Glorp` constructor takes a
-- foreign key into the \"abc\" table.
--
-- Note the _kind_ of `fk` here is @ Symbol -> * @ .
data Widget fk
  = Foo1 Foo
  | Foo2 [Foo]
  | Blarg (Bool, Bool, ByteString)
  | Bleeble Text
  | Glorp (fk "abc")
  deriving (Generic)
-- |
-- There's no TemplateHaskell helper for declaring an `EntityPart` instance for a datastructure
-- parameterized by the foreign key. Thankfully, it's pretty easy to declare manually.
instance EntityPart fk (Widget fk) where
  type StructureOf (Widget fk) = GStructureOf (Rep (Widget fk))

-- | To declare `Eq` and `Show` for a continuation kind, we first need a `Conkin.Functor` instance.
instance Conkin.Functor Widget where
  fmap fn = \case
    Foo1 x -> Foo1 x
    Foo2 x -> Foo2 x
    Blarg x -> Blarg x
    Bleeble x -> Bleeble x
    Glorp x -> Glorp (fn x)

-- | To use the `Eq` TH helper for Widget, we first need a `GEq` instance for all of its fields.
instance GEq Foo where geq = (==)
-- | Declare `Eq` instance using helper from "PersistWrap.Conkin.Extra.TH".
$(deriveFnEq [t| Widget |])

-- | To use the `Show` TH helper for Widget, we first need a `GShow` instance for all of its fields.
instance GShow Foo where gshowsPrec = showsPrec
-- | Declare `Show` instance using helper from "PersistWrap.Conkin.Extra.TH".
$(deriveFnShow [t| Widget |])
