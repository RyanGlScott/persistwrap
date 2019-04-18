{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PersistWrap.Structure.Type where

import Control.Applicative ((<|>), empty)
import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as Text
import Test.QuickCheck

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
    deriving (Eq, Ord, Show)
  |])

data StructTag =
  PrimitiveC | ForeignC | UnitTypeC | SumTypeC | ProductTypeC | ListTypeC | MapTypeC
  deriving (Eq,Bounded,Enum)

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
  arbitrary =
    sized
        (\s ->
          elements (if s <= 1 then terminals else [minBound .. maxBound]) `suchThat` (/= ForeignC)
        )
      >>= \case
            PrimitiveC -> Primitive <$> arbitrary
            ForeignC   -> error "Unreachable"
            UnitTypeC  -> pure UnitType
            SumTypeC   -> SumType <$> do
              subSize <- sized $ fmap (max 1) . geometricBounded (1 % 2)
              NonEmpty.fromList <$> scale
                (\s -> max 0 $ (s - 1) `quot` subSize)
                (zip [ Text.pack ("Con" ++ show i) | i <- [(0 :: Int) ..] ] <$> vector subSize)
            ProductTypeC -> ProductType <$> do
              subSize <- sized $ fmap (max 1) . geometricBounded (1 % 2)
              scale
                (\s -> max 0 $ (s - 1) `quot` subSize)
                (zip [ Text.pack ("field" ++ show i) | i <- [(0 :: Int) ..] ] <$> vector subSize)
            ListTypeC -> scale (\s -> floorSqrt $ max 0 $ s - 1) $ ListType <$> arbitrary
            MapTypeC ->
              scale (\s -> floorSqrt (max 0 (s - 1)) `quot` 2) $ MapType <$> arbitrary <*> arbitrary
  shrink = \case
    Primitive x    -> Primitive <$> shrink x
    Foreign   _    -> empty
    UnitType       -> empty
    SumType     xs -> (snd <$> NonEmpty.toList xs) <|> (SumType <$> shrinkValues xs)
    ProductType xs -> (snd <$> xs) <|> (ProductType <$> shrinkValues xs)
    ListType    x  -> pure x <|> (ListType <$> shrink x)
    MapType k v -> pure k <|> pure v <|> ((`MapType` v) <$> shrink k) <|> (MapType k <$> shrink v)

newtype Unshrinkable a = Unshrinkable {unUnshrinkable :: a}
instance Arbitrary (Unshrinkable a) where
  arbitrary = error "Should not be used"
  shrink    = const []

shrinkValues :: (Functor f, Arbitrary (f (Unshrinkable a, b))) => f (a, b) -> [f (a, b)]
shrinkValues = map (fmap (first unUnshrinkable)) . shrink . fmap (first Unshrinkable)

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . (fromIntegral :: Int -> Float)
