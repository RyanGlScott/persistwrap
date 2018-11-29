{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Consin.Tagged
    ( compareAlwaysSTags
    , eqAlwaysSTags
    , leftTag
    , pickSide
    , rightTag
    , withAlwaysSTaggedShow
    ) where

import Prelude hiding (Functor(..))

import Conkin (Tagged(..))
import Data.Singletons (SingI, sing)
import Data.Singletons.Prelude (type (++), SList, Sing(SCons, SNil), SingInstance(..), singInstance)

import Conkin.Extra (noHere)
import Consin.Class (AlwaysS, Functor(..), compare1, (==*), withAlwaysS)

pickSide
  :: forall (xs :: [k]) (ys :: [k]) (f :: k -> *)
   . SingI xs
  => Tagged (xs ++ ys) f
  -> Either (Tagged xs f) (Tagged ys f)
pickSide l = case sing @_ @xs of
  SNil -> Right l
  SCons _ (singInstance -> SingInstance) -> case l of
    Here  x    -> Left $ Here x
    There rest -> case pickSide rest of
      Left  lres -> Left $ There lres
      Right rres -> Right rres

leftTag :: forall xs ys f . Tagged xs f -> Tagged (xs ++ ys) f
leftTag = \case
  Here  x                   -> Here x
  There (x :: Tagged xs' f) -> There $ leftTag @xs' @ys x

rightTag :: forall xs ys f . SingI xs => Tagged ys f -> Tagged (xs ++ ys) f
rightTag = case sing @_ @xs of
  SNil -> id
  SCons _ ((singInstance -> SingInstance) :: SList xs') -> There . rightTag @xs' @ys

compareAlwaysSTags
  :: forall xs f . (AlwaysS Ord f, SingI xs) => Tagged xs f -> Tagged xs f -> Ordering
compareAlwaysSTags xs ys = case (sing @_ @xs, xs, ys) of
  ((singInstance -> SingInstance) `SCons` _, Here x, Here y) -> compare1 x y
  (_, Here{} , There{}) -> LT
  (_, There{}, Here{} ) -> GT
  (_ `SCons` (singInstance -> SingInstance), There x, There y) -> compareAlwaysSTags x y

eqAlwaysSTags :: forall xs f . (AlwaysS Eq f, SingI xs) => Tagged xs f -> Tagged xs f -> Bool
eqAlwaysSTags xs ys = case (sing @_ @xs, xs, ys) of
  ((singInstance -> SingInstance) `SCons` _, Here x, Here y) -> x ==* y
  (_, Here{} , There{}) -> False
  (_, There{}, Here{} ) -> False
  (_ `SCons` (singInstance -> SingInstance), There x, There y) -> eqAlwaysSTags x y

withAlwaysSTaggedShow
  :: forall xs f y . (SingI xs, AlwaysS Show f) => (Show (Tagged xs f) => y) -> y
withAlwaysSTaggedShow = go (sing @_ @xs)
  where
    go :: forall xs' . SList xs' -> (Show (Tagged xs' f) => y) -> y
    go = \case
      SNil -> id
      (((singInstance -> SingInstance) :: Sing x) `SCons` xs) ->
        \cont -> withAlwaysS @Show @f @x sing $ go xs cont

instance SingI xs => Functor (Tagged xs) where
  fmapSing :: forall a b . (forall x . SingI x => a x -> b x) -> Tagged xs a -> Tagged xs b
  fmapSing fn = go sing
    where
      go :: forall xs' . SList xs' -> Tagged xs' a -> Tagged xs' b
      go ((singInstance -> SingInstance) `SCons` _  ) (Here  x) = Here (fn x)
      go (_                              `SCons` sxs) (There x) = There $ go sxs x
      go SNil x = noHere x
