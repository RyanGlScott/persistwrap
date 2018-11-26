{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PersistWrap.Singletons.Orphans () where

import Data.Singletons (Demote, SingKind, SomeSing(SomeSing), fromSing, toSing)

import Test.QuickCheck (Arbitrary(..))

instance (SingKind x, Arbitrary (Demote x)) => Arbitrary (SomeSing x) where
  arbitrary = toSing <$> arbitrary
  shrink (SomeSing x) = map toSing $ shrink $ fromSing x
