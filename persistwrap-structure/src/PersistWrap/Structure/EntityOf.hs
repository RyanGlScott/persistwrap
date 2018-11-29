{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Structure.EntityOf
    ( EntityOf(..)
    , EntityOfSnd(..)
    , fmapFK
    , foreignKey
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Arrow ((***))
import Data.Constraint (Dict(Dict))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Singletons (SingI, SingInstance(SingInstance), sing, singInstance, withSingI)
import Data.Singletons.Decide (Decision(..), (:~:)(..), (%~))
import Data.Singletons.Prelude (SList, Sing(..))
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits (SSymbol, Symbol)

import Consin
import PersistWrap.Primitives (PrimType, deriveConstraint)
import PersistWrap.Structure.Type

data EntityOf (fk :: Symbol -> *) (struct :: Structure Symbol) where
  Prim :: PrimType p -> EntityOf fk ('Primitive p)
  ForeignKey :: SingI name => fk name -> EntityOf fk ('Foreign name)
  Unit :: EntityOf fk 'UnitType
  Sum :: Tagged (x ': xs) (EntityOfSnd fk) -> EntityOf fk ('SumType (x ':| xs))
  Product :: Tuple xs (EntityOfSnd fk) -> EntityOf fk ('ProductType xs)
  List :: [EntityOf fk x] -> EntityOf fk ('ListType x)
  Map :: Map (EntityOf fk k) (EntityOf fk v) -> EntityOf fk ('MapType k v)

fmapFK
  :: forall fk1 fk2 struct
   . (SingI struct, AlwaysS Eq fk2, AlwaysS Ord fk2)
  => (forall name . SingI name => fk1 name -> fk2 name)
  -> EntityOf fk1 struct
  -> EntityOf fk2 struct
fmapFK fn = go (sing @_ @struct)
  where
    go :: forall struct' . SStructure struct' -> EntityOf fk1 struct' -> EntityOf fk2 struct'
    go = \case
      SPrimitive{} -> \case
        Prim p -> Prim p
      SForeign{} -> \case
        ForeignKey fk -> ForeignKey $ fn fk
      SUnitType -> \case
        Unit -> Unit
      SSumType (xHead :%| xTail) -> withSingI (xHead `SCons` xTail) $ \case
        Sum xs -> Sum $ fmapSing (fmapFKSnd fn) xs
      SProductType (singInstance -> SingInstance) -> \case
        Product xs -> Product $ fmapSing (fmapFKSnd fn) xs
      SListType sx -> \case
        List xs -> List $ map (go sx) xs
      SMapType sk@(singInstance -> SingInstance) sv -> \case
        Map xs -> Map $ Map.fromList $ map (go sk *** go sv) $ Map.toList xs

fmapFKSnd
  :: forall fk1 fk2 nx
   . (SingI nx, AlwaysS Eq fk2, AlwaysS Ord fk2)
  => (forall name . SingI name => fk1 name -> fk2 name)
  -> EntityOfSnd fk1 nx
  -> EntityOfSnd fk2 nx
fmapFKSnd fn (EntityOfSnd x) = case sing @_ @nx of
  STuple2 _ (singInstance -> SingInstance) -> EntityOfSnd $ fmapFK fn x

instance (SingI struct, AlwaysS Show fk) => Show (EntityOf fk struct) where
  showsPrec d = case sing @_ @struct of
    SPrimitive pn -> deriveConstraint @Show pn $ \case
      Prim p -> showParen (d > 10) $ showString "Prim " . showsPrec 11 p
    SForeign (singInstance -> SingInstance) -> \case
      ForeignKey k -> showParen (d > 10) $ showString "ForeignKey " . showsPrec1 11 k
    SUnitType -> \case
      Unit -> showString "Unit"
    SProductType ((singInstance -> SingInstance) :: SList xs) -> \case
      Product xs -> showParen (d > 10) $
        showString "Product " . withAlwaysSTupleShow @xs @(EntityOfSnd fk) showsPrec 11 xs
    SSumType ((sx :: Sing x) :%| (sxs :: SList xs)) -> \case
      Sum xs -> showParen (d > 10) $ withSingI (sx `SCons` sxs) $
        showString "Sum " . withAlwaysSTaggedShow @(x ': xs) @(EntityOfSnd fk) showsPrec 11 xs
    SListType (singInstance -> SingInstance) -> \case
      List xs -> showParen (d > 10) $ showString "List " . showsPrec 11 xs
    SMapType (singInstance -> SingInstance) (singInstance -> SingInstance) -> \case
      Map kvs -> showParen (d > 10) $ showString "Map " . showsPrec 11 kvs
instance AlwaysS Show fk => AlwaysS Show (EntityOf fk) where
  withAlwaysS (singInstance -> SingInstance) = id
instance (SingI nx, AlwaysS Show fk) => Show (EntityOfSnd fk nx) where
  showsPrec d (EntityOfSnd x) = case sing @_ @nx of
    STuple2 _ (singInstance -> SingInstance) -> showParen (d > 10) $
      showString "EntityOfSnd " . showsPrec 11 x
instance AlwaysS Show fk => AlwaysS Show (EntityOfSnd fk) where
  withAlwaysS (singInstance -> SingInstance) = id

instance AlwaysS Eq fk => HEq (EntityOf fk) where
  heq (x :: EntityOf fk x) (y :: EntityOf fk y) = case sing @_ @x %~ sing @_ @y of
    Proved Refl -> if x == y then Just Dict else Nothing
    Disproved{} -> Nothing

foreignKey :: SSymbol name -> fk name -> EntityOf fk ( 'Foreign name)
foreignKey (singInstance -> SingInstance) = ForeignKey

instance (AlwaysS Eq fk, SingI struct) => Eq (EntityOf fk struct) where
  (==) (Prim x) (Prim y) = case sing @_ @struct of
    (SPrimitive pn) -> deriveConstraint @Eq pn (==) x y
  (==) (ForeignKey x) (ForeignKey y) = x ==* y
  (==) Unit Unit = True
  (==) (Sum x) (Sum y) = case sing @_ @struct of
    (SSumType (sx :%| sxs)) -> withSingI (sx `SCons` sxs) eqAlwaysSTags x y
  (==) (Product x) (Product y) = case sing @_ @struct of
    (SProductType (singInstance -> SingInstance)) -> eqAlwaysSTuples x y
  (==) (List x) (List y) = case sing @_ @struct of
    SListType (singInstance -> SingInstance) -> x == y
  (==) (Map x) (Map y) = case sing @_ @struct of
    SMapType (singInstance -> SingInstance) (singInstance -> SingInstance) -> x == y

instance (SingI struct, AlwaysS Eq fk, AlwaysS Ord fk) => Ord (EntityOf fk struct) where
  compare (Prim x) (Prim y) = case sing @_ @struct of
    (SPrimitive pn) -> deriveConstraint @Ord pn compare x y
  compare (ForeignKey x) (ForeignKey y) = compare1 x y
  compare Unit Unit = EQ
  compare (Sum x) (Sum y) = case sing @_ @struct of
    (SSumType (sx :%| sxs)) -> withSingI (sx `SCons` sxs) compareAlwaysSTags x y
  compare (Product x) (Product y) = case sing @_ @struct of
    (SProductType (singInstance -> SingInstance)) -> compareAlwaysSTuples x y
  compare (List x) (List y) = case sing @_ @struct of
    SListType (singInstance -> SingInstance) -> compare x y
  compare (Map x) (Map y) = case sing @_ @struct of
    SMapType (singInstance -> SingInstance) (singInstance -> SingInstance) -> compare x y

data EntityOfSnd fk x where
  EntityOfSnd :: EntityOf fk struct -> EntityOfSnd fk '(sym, struct)

instance (AlwaysS Eq fk, SingI x) => Eq (EntityOfSnd fk x) where
  (==) (EntityOfSnd x) (EntityOfSnd y) = case sing @_ @x of
    STuple2 _ (singInstance -> SingInstance) -> x == y
instance AlwaysS Eq fk => AlwaysS Eq (EntityOfSnd fk) where
  withAlwaysS (singInstance -> SingInstance) = id
instance (AlwaysS Eq fk, AlwaysS Ord fk, SingI x) => Ord (EntityOfSnd fk x) where
  compare (EntityOfSnd x) (EntityOfSnd y) = case sing @_ @x of
    STuple2 _ (singInstance -> SingInstance) -> compare x y
instance (AlwaysS Eq fk, AlwaysS Ord fk) => AlwaysS Ord (EntityOfSnd fk) where
  withAlwaysS (singInstance -> SingInstance) = id
