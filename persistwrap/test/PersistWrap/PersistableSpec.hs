{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PersistWrap.PersistableSpec
    ( spec
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Applicative (empty, (<|>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Singletons (SingI, withSingI)
import Data.Singletons.Prelude (SList, Sing(SCons, SNil, STuple2))
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits (SSymbol, Symbol)
import Test.Hspec
import Test.QuickCheck (Arbitrary(..))

import Consin (AlwaysS)
import PersistWrap
import PersistWrap.Functor.Extra ((<&>))

data Operation fk where
  Insert :: SingI structure => SSymbol name -> EntityOf fk structure -> Operation fk
  Lookup :: SSymbol name -> Int -> Operation fk

shrinkOperation
  :: (AlwaysS Arbitrary fk, AlwaysS Eq fk, AlwaysS Ord fk) => Operation fk -> [Operation fk]
shrinkOperation = \case
  Insert n x -> Insert n <$> shrink x
  Lookup{}   -> empty

data StructureShrink structure where
  StructureShrink
    :: SStructure newStructure
    -> (forall fk. (AlwaysS Eq fk, AlwaysS Ord fk)
        => EntityOf fk structure -> Maybe (EntityOf fk newStructure)
        )
    -> StructureShrink structure

buildShrinks :: SStructure structure -> [StructureShrink structure]
buildShrinks = \case
  SPrimitive{}              -> empty
  SForeign{}                -> empty
  SUnitType                 -> empty
  SSumType     (sx :%| sxs) -> sumToStructure <$> sumShrinks (sx `SCons` sxs)
  SProductType sxs          -> productToStructure <$> productShrinks sxs
  SListType    sx           -> listShrinks <$> buildShrinks sx
  SMapType sk sv ->
    (mapKeyShrinks <$> buildShrinks sk <*> pure sv)
      <|> (mapValShrinks <$> pure sk <*> buildShrinks sv)

data SumShrink (nxs :: [(Symbol, Structure Symbol)]) where
  SumShrink
    :: SList ((newNX ': newNXs) :: [(Symbol, Structure Symbol)])
    -> (forall fk. (AlwaysS Eq fk, AlwaysS Ord fk)
        => Tagged nxs (EntityOfSnd fk) -> Maybe (Tagged (newNX ': newNXs) (EntityOfSnd fk))
        )
    -> SumShrink nxs

sumToStructure :: SumShrink (nx ': nxs) -> StructureShrink ( 'SumType (nx ':| nxs))
sumToStructure (SumShrink (sx `SCons` sxs) fn) =
  StructureShrink (SSumType (sx :%| sxs)) (\(Sum x) -> Sum <$> fn x)

sumShrinks :: forall nxs . SList nxs -> [SumShrink nxs]
sumShrinks = \case
  SNil -> empty
  snx@(STuple2 sn (sx :: SStructure nx)) `SCons` (sxs :: SList nxs') ->
    removeInitial <|> (shrinkInitial <$> buildShrinks sx) <|> shrinkLater sxs
    where
      removeInitial = case sxs of
        SNil               -> empty
        (sx' `SCons` sxs') -> pure $ SumShrink
          (sx' `SCons` sxs')
          (\case
            Here{}  -> Nothing
            There x -> Just x
          )
      shrinkInitial :: StructureShrink nx -> SumShrink nxs
      shrinkInitial (StructureShrink sx' fn) = SumShrink
        (STuple2 sn sx' `SCons` sxs)
        (\case
          Here  (EntityOfSnd x) -> Here . EntityOfSnd <$> fn x
          There xs              -> Just $ There xs
        )
      shrinkLater :: SList nxs' -> [SumShrink nxs]
      shrinkLater sxs' = sumShrinks sxs' <&> \(SumShrink snexs fn) ->
        SumShrink (snx `SCons` snexs) $ \case
          Here  x -> Just $ Here x
          There x -> There <$> fn x

data ProductShrink nxs where
  ProductShrink
    :: SList (newNXs :: [(Symbol, Structure Symbol)])
    -> (forall fk. (AlwaysS Eq fk, AlwaysS Ord fk)
        => Tuple nxs (EntityOfSnd fk) -> Maybe (Tuple newNXs (EntityOfSnd fk))
        )
    -> ProductShrink nxs

productToStructure :: ProductShrink nxs -> StructureShrink ( 'ProductType nxs)
productToStructure (ProductShrink sxs fn) =
  StructureShrink (SProductType sxs) (\(Product x) -> Product <$> fn x)

productShrinks :: forall (nxs :: [(Symbol, Structure Symbol)]) . SList nxs -> [ProductShrink nxs]
productShrinks = \case
  SNil -> empty
  snx@(STuple2 sn (sx :: SStructure nx)) `SCons` (sxs :: SList nxs') ->
    pure removeInitial
      <|> (shrinkInitial <$> buildShrinks sx)
      <|> (shrinkLater <$> productShrinks sxs)
    where
      removeInitial :: ProductShrink nxs
      removeInitial = ProductShrink sxs (\(_ `Cons` xs) -> Just xs)
      shrinkInitial :: StructureShrink nx -> ProductShrink nxs
      shrinkInitial (StructureShrink sx' fn) = ProductShrink
        (STuple2 sn sx' `SCons` sxs)
        (\(EntityOfSnd x `Cons` xs) -> (`Cons` xs) . EntityOfSnd <$> fn x)
      shrinkLater :: ProductShrink nxs' -> ProductShrink nxs
      shrinkLater (ProductShrink sxs' fn) =
        ProductShrink (snx `SCons` sxs') (\(x `Cons` xs) -> (x `Cons`) <$> fn xs)

listShrinks :: forall x . StructureShrink x -> StructureShrink ( 'ListType x)
listShrinks (StructureShrink x fn) =
  StructureShrink (SListType x) (\(List xs) -> Just $ List $ mapMaybe fn xs)

mapKeyShrinks :: forall k v . StructureShrink k -> SStructure v -> StructureShrink ( 'MapType k v)
mapKeyShrinks (StructureShrink sk fn) sv = StructureShrink
  (SMapType sk sv)
  (\(Map (Map.toList -> kvs)) ->
    Just $ Map $ withSingI sk $ Map.fromList $ mapMaybe (\(k, v) -> (, v) <$> fn k) kvs
  )

mapValShrinks :: forall k v . SStructure k -> StructureShrink v -> StructureShrink ( 'MapType k v)
mapValShrinks sk (StructureShrink sv fn) = StructureShrink
  (SMapType sk sv)
  (\(Map (Map.toList -> kvs)) ->
    Just $ Map $ withSingI sk $ Map.fromList $ mapMaybe (\(k, v) -> (k, ) <$> fn v) kvs
  )

spec :: Spec
spec = describe "Insertion and lookup" $ it "should behave like the model" pending
