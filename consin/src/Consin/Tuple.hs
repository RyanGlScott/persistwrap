{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Consin.Tuple
    ( (++&)
    , compareAlwaysSTuples
    , eqAlwaysSTuples
    , mapUncheckSing
    , singToTuple
    , splitTuple
    , zipUncheckSing
    ) where

import Prelude hiding (Functor(..), unzip)

import Conkin (Tuple(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Singletons (Sing, SingI, SingInstance(SingInstance), sing, singInstance)
import Data.Singletons.Prelude (type (++), SList, Sing(SCons, SNil))
import Test.QuickCheck (Arbitrary(..))

import Conkin.Extra (htraverse)
import Consin.Class (AlwaysS, ConsinShow(..), Functor(..), compare1, (==*), withAlwaysS)

(++&) :: Tuple xs f -> Tuple ys f -> Tuple (xs ++ ys) f
(++&) Nil         t  = t
(++&) (Cons x xs) ys = Cons x (xs ++& ys)

splitTuple :: forall xs ys f . SingI xs => Tuple (xs ++ ys) f -> (Tuple xs f, Tuple ys f)
splitTuple t = case sing @_ @xs of
  SNil -> (Nil, t)
  SCons _ ((singInstance -> SingInstance) :: SList xs') -> case t of
    (v `Cons` vs) -> case splitTuple @xs' @ys vs of
      (l, r) -> (v `Cons` l, r)

singToTuple :: SList xs -> Tuple xs Sing
singToTuple = \case
  SNil       -> Nil
  SCons x xs -> Cons x (singToTuple xs)

instance SingI xs => Functor (Tuple xs) where
  fmapSing :: forall a b . (forall x . SingI x => a x -> b x) -> Tuple xs a -> Tuple xs b
  fmapSing fn = go sing
    where
      go :: forall xs' . SList xs' -> Tuple xs' a -> Tuple xs' b
      go SNil Nil           = Nil
      go ((singInstance -> SingInstance) `SCons` sxs) (x `Cons` xs) = fn x `Cons` go sxs xs

mapUncheckSing :: forall a xs y . SList xs -> (forall x . SingI x => a x -> y) -> Tuple xs a -> [y]
mapUncheckSing s fn = go s
  where
    go :: forall xs' . SList xs' -> Tuple xs' a -> [y]
    go SNil Nil           = []
    go ((singInstance -> SingInstance) `SCons` sxs) (x `Cons` xs) = fn x : go sxs xs

zipUncheckSing
  :: forall a b xs y
   . SingI xs
  => (forall x . SingI x => a x -> b x -> y)
  -> Tuple xs a
  -> Tuple xs b
  -> [y]
zipUncheckSing fn = go sing
  where
    go :: forall xs' . SList xs' -> Tuple xs' a -> Tuple xs' b -> [y]
    go SNil Nil Nil = []
    go ((singInstance -> SingInstance) `SCons` sxs) (x `Cons` xs) (y `Cons` ys) =
      fn x y : go sxs xs ys

compareAlwaysSTuples :: (SingI xs, AlwaysS Ord f) => Tuple xs f -> Tuple xs f -> Ordering
compareAlwaysSTuples x y = fromMaybe EQ $ find (/= EQ) $ zipUncheckSing compare1 x y

eqAlwaysSTuples :: (SingI xs, AlwaysS Eq f) => Tuple xs f -> Tuple xs f -> Bool
eqAlwaysSTuples x y = and $ zipUncheckSing (==*) x y

withAlwaysSTupleShow :: forall xs f y . (SingI xs, AlwaysS Show f) => (Show (Tuple xs f) => y) -> y
withAlwaysSTupleShow = go (sing @_ @xs)
  where
    go :: forall xs' . SList xs' -> (Show (Tuple xs' f) => y) -> y
    go = \case
      SNil -> id
      (((singInstance -> SingInstance) :: Sing x) `SCons` xs) ->
        \cont -> withAlwaysS @Show @f @x sing $ go xs cont

instance (SingI xs, AlwaysS Show f) => ConsinShow (Tuple xs f) where
  showsPrecS = withAlwaysSTupleShow @xs @f showsPrec

instance (SingI xs, AlwaysS Arbitrary f) => Arbitrary (Tuple xs f) where
  arbitrary =
    htraverse
        (\((singInstance -> SingInstance) :: Sing x) -> withAlwaysS @Arbitrary @f @x sing arbitrary)
      $ singToTuple (sing @_ @xs)
  shrink = go (sing @_ @xs)
    where
      go :: forall xs' . SList xs' -> Tuple xs' f -> [Tuple xs' f]
      go SNil Nil = []
      go (((singInstance -> SingInstance) :: Sing x) `SCons` sxs) (x `Cons` xs) =
        map (`Cons` xs) (withAlwaysS @Arbitrary @f @x sing shrink x) ++ map (x `Cons`) (go sxs xs)
