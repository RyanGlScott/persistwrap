{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.SpecUtil.Widget where

import qualified Conkin
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic, Rep)

import PersistWrap.Conkin.Extra.TH (deriveFnEq, deriveFnShow)
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

data Widget fk
  = Foo1 Foo
  | Foo2 [Foo]
  | Blarg (Bool, Bool, ByteString)
  | Bleeble Text
  | Glorp (fk "abc")
  deriving (Generic)
instance EntityPart fk (Widget fk) where
  type StructureOf (Widget fk) = GStructureOf (Rep (Widget fk))
instance Conkin.Functor Widget where
  fmap fn = \case
    Foo1 x -> Foo1 x
    Foo2 x -> Foo2 x
    Blarg x -> Blarg x
    Bleeble x -> Bleeble x
    Glorp x -> Glorp (fn x)
$(deriveFnEq [t| Widget |])
$(deriveFnShow [t| Widget |])
