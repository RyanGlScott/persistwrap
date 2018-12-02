module PersistWrap.Persistable.Rep
    ( ColumnRep(..)
    , NamedColumnRep(..)
    , NamedSchemaRep(..)
    , SchemaRep(..)
    , getSchemaRep
    ) where

import Prelude hiding ((.))

import Conkin (Tagged(..), Tuple(..))
import qualified Conkin
import Control.Category ((.))
import Data.Bijection
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(Proxy))
import Data.Singletons (withSingI)
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits

import Conkin.Extra
import PersistWrap.Maybe.Extra (fromJust)
import PersistWrap.Persistable.Columns (sKeyColumnName)
import PersistWrap.Persistable.Rep.Utils
import PersistWrap.Structure as Structure
import PersistWrap.Table as Table

getSchemaRep :: SSymbol schemaName -> SStructure structure -> NamedSchemaRep fk schemaName structure
getSchemaRep prefix = NamedSchemaRep prefix . either AtMostOneColumnSchema id . buildRep prefix

type BuildRepResult fk structure =
  Either (ColumnRep fk (EntityOf fk structure)) (SchemaRep fk structure)

buildProductRep
  :: forall fk schemaName xs
   . SSymbol schemaName
  -> SList (xs :: [(Symbol, Structure Symbol)])
  -> BuildRepResult fk ( 'ProductType xs)
buildProductRep prefix xs = case nonUnitsCount of
  0 -> Left $ UnitRep $ Product $ Conkin.fmap (fromJust . extractUnit) colReps
  1 -> Left $ case buildSingleProductCol colReps of
    SingleCol cr fn -> bimapCol (biProduct . fn) cr
  _ -> Right $ ProductSchema colReps
  where
    colReps       = buildColReps prefix xs
    nonUnitsCount = length $ filter not $ mapUncheck isUnit colReps

buildColRep :: SSymbol schemaName -> SStructure structure -> ColumnRep fk (EntityOf fk structure)
buildColRep prefix structure = case buildRep prefix structure of
  Left  cr -> cr
  Right sr -> ForeignRep (NamedSchemaRep prefix sr)

buildColReps
  :: forall fk schemaName xs
   . SSymbol schemaName
  -> SList (xs :: [(Symbol, Structure Symbol)])
  -> Tuple xs (NamedColumnRep fk)
buildColReps prefix = go
  where
    go :: forall xs' . SList xs' -> Tuple xs' (NamedColumnRep fk)
    go = \case
      SNil                      -> Nil
      STuple2 name x `SCons` xs -> case go xs of
        rtup -> NamedColumnRep name (buildColRep (addName prefix name) x) `Cons` rtup

buildNonEmptyColReps
  :: forall fk schemaName x xs
   . SSymbol schemaName
  -> SList ((x ': xs) :: [(Symbol, Structure Symbol)])
  -> Tuple (x ': xs) (NamedColumnRep fk)
buildNonEmptyColReps prefix (STuple2 name x `SCons` xs) =
  NamedColumnRep name (buildColRep (addName prefix name) x) `Cons` buildColReps prefix xs

buildSumRep
  :: forall fk schemaName x xs
   . SSymbol schemaName
  -> SList (x ': xs :: [(Symbol, Structure Symbol)])
  -> BuildRepResult fk ( 'SumType (x ':| xs))
buildSumRep prefix xs = case colReps of
  NamedColumnRep _ cr `Cons` Nil ->
    Left $ bimapCol (biSum . Bi Here getSingle . Bi EntityOfSnd (\(EntityOfSnd v) -> v)) cr
  _ -> case (unitsCount, nonUnitsCount) of
    (_, 0)                   -> Left $ buildEnumCol colReps
    (1, 1)                   -> Left $ buildOptionCol colReps
    (_, _) | unitsCount <= 1 -> Right $ SumUnIndexedSchema
      (findJust (\x -> if hasUnitOption x then Just Proxy else Nothing) colReps)
      allNullColReps
    (_, _) -> Right $ SumIndexedSchema allNullColReps
  where
    colReps        = buildNonEmptyColReps prefix xs
    unitsCount     = length $ filter id $ mapUncheck hasUnitOption colReps
    nonUnitsCount  = length $ filter not $ mapUncheck isUnit colReps
    allNullColReps = Conkin.fmap namedUnderlyingNullable colReps

buildRep :: SSymbol schemaName -> SStructure structure -> BuildRepResult fk structure
buildRep prefix = \case
  SPrimitive pn -> Left $ FnRep
    (PrimRep (SColumn SFalse (SPrim pn)))
    (Bi (\(PV x) -> Structure.Prim x) (\(Structure.Prim x) -> PV x) . inverse biV)
  SForeign name -> Left $ FnRep
    (PrimRep (SColumn SFalse (SForeignKey name)))
    ( Bi (\(FKV fk) -> Structure.foreignKey name fk) (\(Structure.ForeignKey fk) -> FKV fk)
    . inverse biV
    )
  SUnitType                 -> Left $ UnitRep Unit
  SProductType nxs          -> buildProductRep prefix nxs
  SSumType     (nx :%| nxs) -> buildSumRep prefix (nx `SCons` nxs)
  SListType    x            -> Left $ ListRep $ getSchemaRep (addName prefix (SSym @"elems")) x
  SMapType k v              -> Left $ withSingI k
                                                MapRep
                                                (buildColRep (addName prefix sKeyColumnName) k)
                                                (getSchemaRep (addName prefix (SSym @"entries")) v)
