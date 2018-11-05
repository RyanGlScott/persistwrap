{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Structure.EntityOf
    ( EntityOf(..)
    , EntityOfSnd(..)
    , KeyOccursIn
    , prim
    , field
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Lens (Lens', (<&>))
import Data.Constraint (Dict(Dict))
import Data.Map (Map)
import Data.Promotion.Prelude.Eq (type (==))
import Data.Proxy (Proxy(Proxy))
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.Prelude (Sing(..))
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Conkin.Extra (Always(..), compareAlwaysTags, compareAlwaysTuples)
import PersistWrap.Structure.Primitives (PrimType, deriveConstraint)
import PersistWrap.Structure.Type

data EntityOf (struct :: Structure Symbol) where
  Prim :: PrimType p -> EntityOf ('Primitive p)
  Unit :: EntityOf 'UnitType
  Sum :: Tagged (x ': xs) EntityOfSnd -> EntityOf ('SumType x xs)
  Product :: Tuple xs EntityOfSnd -> EntityOf ('ProductType xs)
  List :: [EntityOf x] -> EntityOf ('ListType x)
  Map :: Map (EntityOf k) (EntityOf v) -> EntityOf ('MapType k v)

instance SingI struct => Eq (EntityOf struct) where
  (==) x y = compare x y == EQ

instance SingI struct => Ord (EntityOf struct) where
  compare (Prim x) (Prim y) = case sing @_ @struct of
    (SPrimitive pn) -> deriveConstraint @Ord pn compare x y
  compare Unit                    Unit                   = EQ
  compare (Sum x)                 (Sum y)                = case sing @_ @struct of
    (SSumType sx sxs) -> withSingI (sx `SCons` sxs) compareAlwaysTags x y
  compare (Product x) (Product y) = case sing @_ @struct of
    (SProductType sxs) -> withSingI sxs compareAlwaysTuples x y
  compare (List x) (List y) = case sing @_ @struct of
    SListType sx -> withSingI sx compare x y
  compare (Map x) (Map y) = case sing @_ @struct of
    SMapType sk sv -> withSingI sk (withSingI sv compare) x y

data EntityOfSnd x where
  EntityOfSnd :: SSymbol sym -> EntityOf struct -> EntityOfSnd '(sym, struct)

instance SingI x => Eq (EntityOfSnd x) where
  (==) (EntityOfSnd _ x) (EntityOfSnd _ y) = case sing @_ @x of
    STuple2 _ sx -> withSingI sx (==) x y
instance Always Eq EntityOfSnd where dict = Dict
instance SingI x => Ord (EntityOfSnd x) where
  compare (EntityOfSnd _ x) (EntityOfSnd _ y) = case sing @_ @x of
    STuple2 _ sx -> withSingI sx compare x y
instance Always Ord EntityOfSnd where dict = Dict

prim :: forall p . Lens' (EntityOf ( 'Primitive p)) (PrimType p)
prim fn (Prim b) = Prim <$> fn b

field
  :: forall sym struct xs
   . KeyOccursIn sym struct xs
  => SSymbol sym
  -> Lens' (EntityOf ( 'ProductType xs)) (EntityOf struct)
field sym fn (Product x) = Product <$> entry sym fn x

class KeyOccursIn (sym :: Symbol) (struct :: Structure Symbol) (xs :: [(Symbol,Structure Symbol)])
    | sym xs -> struct where
  entry :: SSymbol sym -> Lens' (Tuple xs EntityOfSnd) (EntityOf struct)

instance SubOccurs (symk == symx) symk structk structx xs
    => KeyOccursIn symk structk ('(symx,structx) ': xs) where
  entry = subEntry (Proxy @(symk == symx)) (Proxy @symx)

class SubOccurs
    (current :: Bool)
    (symk :: Symbol)
    (structk :: Structure Symbol)
    (structx :: Structure Symbol)
    (xs :: [(Symbol,Structure Symbol)])
    | current symk structx xs -> structk where
  subEntry :: forall symx
    . Proxy current
    -> Proxy symx
    -> SSymbol symk
    -> Lens' (Tuple ( '(symx , structx) ': xs) EntityOfSnd) (EntityOf structk)

instance structk ~ structx => SubOccurs 'True symk structk structx xs where
  subEntry _ _ _ fn (EntityOfSnd k x `Cons` xs) = fn x <&> \x' -> EntityOfSnd k x' `Cons` xs

instance KeyOccursIn symk structk xs => SubOccurs 'False symk structk structx xs where
  subEntry _ _ symk fn (x `Cons` xs) = (x `Cons`) <$> entry symk fn xs
