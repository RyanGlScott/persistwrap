{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.Type where

import Control.Arrow ((&&&))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
import Data.Singletons.TH (singletons)
import Data.Text (Text)
import Test.QuickCheck (Arbitrary(..), Gen, choose, elements, scale, sized, vector)

import PersistWrap.Primitives

$(singletons [d|
  data Structure text
    = Primitive PrimName
    | Foreign text
    | UnitType
    | SumType (NonEmpty (text, Structure text))
    | ProductType [(text, Structure text)]
    | ListType (Structure text)
    | MapType (Structure text) (Structure text)
  |])

data StructTag =
  PrimitiveC | ForeignC | UnitTypeC | SumTypeC | ProductTypeC | ListTypeC | MapTypeC
  deriving (Bounded,Enum)

_constructorOf :: Structure text -> StructTag
_constructorOf = \case
  Primitive{}   -> PrimitiveC
  Foreign{}     -> ForeignC
  UnitType{}    -> UnitTypeC
  SumType{}     -> SumTypeC
  ProductType{} -> ProductTypeC
  ListType{}    -> ListTypeC
  MapType{}     -> MapTypeC

terminals :: [StructTag]
terminals = [PrimitiveC, ForeignC, UnitTypeC, ProductTypeC]

geometricBounded :: Rational -> Int -> Gen Int
geometricBounded ((fromInteger . numerator) &&& (fromInteger . denominator) -> (num, denom)) = go
  where
    go bound
      | bound <= 0 = return 0
      | otherwise = do
        x <- choose (0 :: Float, 1)
        if x * denom < num then return 0 else (1 +) <$> go (bound - 1)

instance Arbitrary (Structure Text) where
  arbitrary = sized $ \s ->
    elements (if s <= 0 then terminals else [minBound .. maxBound]) >>= \case
      PrimitiveC -> Primitive <$> arbitrary
      ForeignC   -> Foreign <$> arbitrary
      UnitTypeC  -> pure UnitType
      SumTypeC   -> SumType <$> do
        subSize <- sized $ fmap (max 1) . geometricBounded (1 % 2)
        NonEmpty.fromList <$> scale (`quot` subSize) (vector subSize)
      ProductTypeC -> ProductType <$> do
        subSize <- sized $ geometricBounded (1 % 2)
        scale (`quot` subSize) (vector subSize)
      ListTypeC -> ListType <$> arbitrary
      MapTypeC  -> scale (`quot` 2) $ MapType <$> arbitrary <*> arbitrary
  shrink = \case
    Primitive x -> map Primitive $ shrink x
    Foreign tabName -> map Foreign $ shrink tabName
    UnitType -> []
    SumType xs -> map SumType $ shrink xs
    ProductType xs -> map ProductType $ shrink xs
    ListType x -> map ListType $ shrink x
    MapType k v -> map (`MapType` v) (shrink k) ++ map (MapType k) (shrink v)
