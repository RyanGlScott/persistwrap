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
import Data.Singletons (SingI, SingInstance(SingInstance), sing, singInstance, withSingI)
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
instance AlwaysS Eq fk => AlwaysS Eq (EntityOfSnd fk) where withAlwaysS = id
instance (AlwaysS Eq fk, AlwaysS Ord fk, SingI x) => Ord (EntityOfSnd fk x) where
  compare (EntityOfSnd x) (EntityOfSnd y) = case sing @_ @x of
    STuple2 _ (singInstance -> SingInstance) -> compare x y
instance (AlwaysS Eq fk, AlwaysS Ord fk) => AlwaysS Ord (EntityOfSnd fk) where withAlwaysS = id

instance (AlwaysS Arbitrary fk, AlwaysS Eq fk, AlwaysS Ord fk, SingI structure)
    => Arbitrary (EntityOf fk structure) where
  arbitrary = case sing @_ @structure of
    SPrimitive pn -> Prim <$> deriveConstraint @Arbitrary pn arbitrary
    SForeign ((singInstance -> SingInstance) :: Sing name) ->
      ForeignKey <$> withAlwaysS @Arbitrary @fk @name arbitrary
    SUnitType -> pure Unit
    SProductType (singInstance -> SingInstance) -> Product <$> arbitrary
    SSumType (x0 :%| xs) -> Sum <$> withSingI (x0 `SCons` xs) arbitrary
    SListType (singInstance -> SingInstance) -> List <$> arbitrary
    SMapType (singInstance -> SingInstance) (singInstance -> SingInstance) -> Map <$> arbitrary
  shrink = case sing @_ @structure of
    SPrimitive pn -> \case
      Prim p -> map Prim $ deriveConstraint @Arbitrary pn $ shrink p
    SForeign (_ :: Sing fkn) -> \case
      ForeignKey fk -> withAlwaysS @Arbitrary @fk @fkn $ map ForeignKey $ shrink fk
    SUnitType -> \case
      Unit -> []
    SProductType (singInstance -> SingInstance) -> \case
      Product xs -> map Product $ shrink xs
    SSumType (sx :%| sxs) -> \case
      Sum xs -> map Sum $ withSingI (sx `SCons` sxs) shrink xs
    SListType (singInstance -> SingInstance) -> \case
      List xs -> map List $ shrink xs
    SMapType (singInstance -> SingInstance) (singInstance -> SingInstance) -> \case
      Map kvm -> map Map $ shrink kvm

instance (AlwaysS Arbitrary fk, AlwaysS Eq fk, AlwaysS Ord fk, SingI nx)
    => Arbitrary (EntityOfSnd fk nx) where
  arbitrary = case sing @_ @nx of
    STuple2 _ (singInstance -> SingInstance) -> EntityOfSnd <$> arbitrary

instance (AlwaysS Arbitrary fk, AlwaysS Eq fk, AlwaysS Ord fk)
    => AlwaysS Arbitrary (EntityOfSnd fk) where
  withAlwaysS = id
