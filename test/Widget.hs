{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Widget where

import qualified Conkin
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

import PersistWrap.Conkin.Extra.TH (deriveFnEq, deriveFnShow)
import PersistWrap.Structure.TH (deriveEntityPart, deriveEntityPartFK)

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
$(deriveEntityPartFK [t| Widget |])
instance Conkin.Functor Widget where
  fmap fn = \case
    Foo1 x -> Foo1 x
    Foo2 x -> Foo2 x
    Blarg x -> Blarg x
    Bleeble x -> Bleeble x
    Glorp x -> Glorp (fn x)
$(deriveFnEq [t| Widget |])
$(deriveFnShow [t| Widget |])
