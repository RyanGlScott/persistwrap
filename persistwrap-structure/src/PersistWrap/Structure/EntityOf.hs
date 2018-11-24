{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Structure.EntityOf
    ( EntityOf(..)
    , EntityOfSnd(..)
    , foreignKey
    ) where

import Conkin (Tagged(..), Tuple(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.Prelude (Sing(..))
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits (SSymbol, Symbol)
import Test.QuickCheck (Arbitrary(..))

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

foreignKey :: SSymbol name -> fk name -> EntityOf fk ( 'Foreign name)
foreignKey name = withSingI name ForeignKey

instance (AlwaysS Eq fk, SingI struct) => Eq (EntityOf fk struct) where
  (==) (Prim x) (Prim y) = case sing @_ @struct of
    (SPrimitive pn) -> deriveConstraint @Eq pn (==) x y
  (==) (ForeignKey x) (ForeignKey y) = x ==* y
  (==) Unit Unit = True
  (==) (Sum x) (Sum y) = case sing @_ @struct of
    (SSumType (sx :%| sxs)) -> withSingI (sx `SCons` sxs) eqAlwaysSTags x y
  (==) (Product x) (Product y) = case sing @_ @struct of
    (SProductType sxs) -> withSingI sxs eqAlwaysSTuples x y
  (==) (List x) (List y) = case sing @_ @struct of
    SListType sx -> withSingI sx (==) x y
  (==) (Map x) (Map y) = case sing @_ @struct of
    SMapType sk sv -> withSingI sk (withSingI sv (==)) x y

instance (SingI struct, AlwaysS Eq fk, AlwaysS Ord fk) => Ord (EntityOf fk struct) where
  compare (Prim x) (Prim y) = case sing @_ @struct of
    (SPrimitive pn) -> deriveConstraint @Ord pn compare x y
  compare (ForeignKey x) (ForeignKey y) = compare1 x y
  compare Unit Unit = EQ
  compare (Sum x) (Sum y) = case sing @_ @struct of
    (SSumType (sx :%| sxs)) -> withSingI (sx `SCons` sxs) compareAlwaysSTags x y
  compare (Product x) (Product y) = case sing @_ @struct of
    (SProductType sxs) -> withSingI sxs compareAlwaysSTuples x y
  compare (List x) (List y) = case sing @_ @struct of
    SListType sx -> withSingI sx compare x y
  compare (Map x) (Map y) = case sing @_ @struct of
    SMapType sk sv -> withSingI sk (withSingI sv compare) x y

data EntityOfSnd fk x where
  EntityOfSnd :: EntityOf fk struct -> EntityOfSnd fk '(sym, struct)

instance (AlwaysS Eq fk, SingI x) => Eq (EntityOfSnd fk x) where
  (==) (EntityOfSnd x) (EntityOfSnd y) = case sing @_ @x of
    STuple2 _ sx -> withSingI sx (==) x y
instance AlwaysS Eq fk => AlwaysS Eq (EntityOfSnd fk) where withAlwaysS = id
instance (AlwaysS Eq fk, AlwaysS Ord fk, SingI x) => Ord (EntityOfSnd fk x) where
  compare (EntityOfSnd x) (EntityOfSnd y) = case sing @_ @x of
    STuple2 _ sx -> withSingI sx compare x y
instance (AlwaysS Eq fk, AlwaysS Ord fk) => AlwaysS Ord (EntityOfSnd fk) where withAlwaysS = id

instance (AlwaysS Arbitrary fk, AlwaysS Eq fk, AlwaysS Ord fk, SingI structure)
    => Arbitrary (EntityOf fk structure) where
  arbitrary = case sing @_ @structure of
    SPrimitive pn -> Prim <$> deriveConstraint @Arbitrary pn arbitrary
    SForeign (fkn :: Sing name) ->
      withSingI fkn $ ForeignKey <$> withAlwaysS @Arbitrary @fk @name arbitrary
    SUnitType -> pure Unit
    SProductType xs -> Product <$> withSingI xs arbitrary
    SSumType (x0 :%| xs) -> Sum <$> withSingI (x0 `SCons` xs) arbitrary
    SListType x  -> List <$> withSingI x arbitrary
    SMapType k v -> Map <$> withSingI k (withSingI v arbitrary)
  shrink = case sing @_ @structure of
    SPrimitive pn -> \case
      Prim p -> map Prim $ deriveConstraint @Arbitrary pn $ shrink p
    SForeign (_ :: Sing fkn) -> \case
      ForeignKey fk -> withAlwaysS @Arbitrary @fk @fkn $ map ForeignKey $ shrink fk
    SUnitType -> \case
      Unit -> []
    SProductType sxs -> \case
      Product xs -> map Product $ withSingI sxs shrink xs
    SSumType (sx :%| sxs) -> \case
      Sum xs -> map Sum $ withSingI (sx `SCons` sxs) shrink xs
    SListType sx -> \case
      List xs -> map List $ withSingI sx shrink xs
    SMapType sk sv -> \case
      Map kvm -> map Map $ withSingI sk (withSingI sv shrink) kvm

instance (AlwaysS Arbitrary fk, AlwaysS Eq fk, AlwaysS Ord fk, SingI nx)
    => Arbitrary (EntityOfSnd fk nx) where
  arbitrary = case sing @_ @nx of
    STuple2 _ x -> EntityOfSnd <$> withSingI x arbitrary

instance (AlwaysS Arbitrary fk, AlwaysS Eq fk, AlwaysS Ord fk)
    => AlwaysS Arbitrary (EntityOfSnd fk) where
  withAlwaysS = id
