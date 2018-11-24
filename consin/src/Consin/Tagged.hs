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
    ) where

import Conkin (Tagged(..))
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.Prelude (type (++), SList, Sing(SCons, SNil))
import Test.QuickCheck (Arbitrary(..), oneof)

import Conkin.Extra (htraverse, noHere, tagCases)
import Consin.Class (AlwaysS, compare1, (==*), withAlwaysS)
import Consin.Tuple (singToTuple)

pickSide
  :: forall (xs :: [k]) (ys :: [k]) (f :: k -> *)
   . SingI xs
  => Tagged (xs ++ ys) f
  -> Either (Tagged xs f) (Tagged ys f)
pickSide l = case sing @_ @xs of
  SNil        -> Right l
  SCons _ xs' -> case l of
    Here  x    -> Left $ Here x
    There rest -> case withSingI xs' pickSide rest of
      Left  lres -> Left $ There lres
      Right rres -> Right rres

leftTag :: forall xs ys f . Tagged xs f -> Tagged (xs ++ ys) f
leftTag = \case
  Here  x                   -> Here x
  There (x :: Tagged xs' f) -> There $ leftTag @xs' @ys x

rightTag :: forall xs ys f . SingI xs => Tagged ys f -> Tagged (xs ++ ys) f
rightTag = case sing @_ @xs of
  SNil                       -> id
  SCons _ (xs' :: SList xs') -> There . withSingI xs' (rightTag @xs' @ys)

compareAlwaysSTags
  :: forall xs f . (AlwaysS Ord f, SingI xs) => Tagged xs f -> Tagged xs f -> Ordering
compareAlwaysSTags xs ys = case (sing @_ @xs, xs, ys) of
  ((sx :: Sing x) `SCons` _, Here x , Here y ) -> withSingI sx compare1 x y
  (_                       , Here{} , There{}) -> LT
  (_                       , There{}, Here{} ) -> GT
  (_ `SCons` sxs           , There x, There y) -> withSingI sxs compareAlwaysSTags x y

eqAlwaysSTags :: forall xs f . (AlwaysS Eq f, SingI xs) => Tagged xs f -> Tagged xs f -> Bool
eqAlwaysSTags xs ys = case (sing @_ @xs, xs, ys) of
  ((sx :: Sing x) `SCons` _, Here x , Here y ) -> withSingI sx (==*) x y
  (_                       , Here{} , There{}) -> False
  (_                       , There{}, Here{} ) -> False
  (_ `SCons` sxs           , There x, There y) -> withSingI sxs eqAlwaysSTags x y

instance (AlwaysS Arbitrary f, SingI (x ': xs)) => Arbitrary (Tagged (x ': xs) f) where
  arbitrary = oneof $ map
    (htraverse (\(sx :: Sing x') -> withSingI sx $ withAlwaysS @Arbitrary @f @x' arbitrary))
    (tagCases (singToTuple (sing @_ @(x ': xs))))
  shrink = go (sing @_ @(x ': xs))
    where
      go :: forall xs' . Sing xs' -> Tagged xs' f -> [Tagged xs' f]
      go ((sx :: Sing x') `SCons` _) (Here x) =
        map Here $ withSingI sx (withAlwaysS @Arbitrary @f @x' shrink x)
      go (_ `SCons` sxs) (There xs) = map There $ go sxs xs
      go SNil            t          = noHere t
