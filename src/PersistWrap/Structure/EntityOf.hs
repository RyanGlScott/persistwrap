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
import Data.Map (Map)
import Data.Promotion.Prelude.Eq (type (==))
import Data.Proxy (Proxy(Proxy))
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Structure.Primitives (PrimType)
import PersistWrap.Structure.Type (Structure(..))

data EntityOf (struct :: Structure) where
  Prim :: PrimType p -> EntityOf ('Primitive p)
  Unit :: EntityOf 'UnitType
  Sum :: Tagged (x ': xs) EntityOfSnd -> EntityOf ('SumType x xs)
  Product :: Tuple xs EntityOfSnd -> EntityOf ('ProductType xs)
  List :: [EntityOf x] -> EntityOf ('ListType x)
  Map :: Map (PrimType k) (EntityOf v) -> EntityOf ('MapType k v)

data EntityOfSnd x where
  EntityOfSnd :: SSymbol sym -> EntityOf struct -> EntityOfSnd '(sym, struct)

prim :: forall p . Lens' (EntityOf ( 'Primitive p)) (PrimType p)
prim fn (Prim b) = Prim <$> fn b

field
  :: forall sym struct xs
   . KeyOccursIn sym struct xs
  => SSymbol sym
  -> Lens' (EntityOf ( 'ProductType xs)) (EntityOf struct)
field sym fn (Product x) = Product <$> entry sym fn x

class KeyOccursIn (sym :: Symbol) (struct :: Structure) (xs :: [(Symbol,Structure)])
    | sym xs -> struct where
  entry :: SSymbol sym -> Lens' (Tuple xs EntityOfSnd) (EntityOf struct)

instance SubOccurs (symk == symx) symk structk structx xs
    => KeyOccursIn symk structk ('(symx,structx) ': xs) where
  entry = subEntry (Proxy @(symk == symx)) (Proxy @symx)

class SubOccurs
    (current :: Bool)
    (symk :: Symbol)
    (structk :: Structure)
    (structx :: Structure)
    (xs :: [(Symbol,Structure)])
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
