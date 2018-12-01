{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Consin.Tagged
    ( compareAlwaysSTags
    , eqAlwaysSTags
    , getTaggedValueS
    , leftTag
    , pickSide
    , rightTag
    ) where

import Prelude hiding (Functor(..))

import Conkin (Tagged(..))
import Data.Constraint (Dict(Dict))
import Data.Singletons (SingI, sing)
import Data.Singletons.Prelude (type (++), SList, Sing(SCons, SNil), SingInstance(..), singInstance)
import Test.QuickCheck (Arbitrary(..), oneof)

import Conkin.Extra (htraverse, noHere, tagCases)
import Consin.Class (AlwaysS, ConsinShow(..), Functor(..), compare1, (==*), withAlwaysS)
import Consin.Tuple (singToTuple)

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

deriveShow :: forall xs f . (SingI xs, AlwaysS Show f) => Dict (Show (Tagged xs f))
deriveShow = go (sing @_ @xs)
  where
    go :: forall xs' . SList xs' -> Dict (Show (Tagged xs' f))
    go = \case
      SNil             -> Dict
      (sx `SCons` sxs) -> case go sxs of
        Dict -> withAlwaysS @Show @f sx Dict

instance (SingI xs, AlwaysS Show f) => ConsinShow (Tagged xs f) where
  showsPrecS = case deriveShow @xs @f of
    Dict -> showsPrec

instance (AlwaysS Arbitrary f, SingI (x ': xs)) => Arbitrary (Tagged (x ': xs) f) where
  arbitrary = oneof $ map
    (htraverse
      (\((singInstance -> SingInstance) :: Sing x') -> withAlwaysS @Arbitrary @f @x' sing arbitrary)
    )
    (tagCases (singToTuple (sing @_ @(x ': xs))))
  shrink = go (sing @_ @(x ': xs))
    where
      go :: forall xs' . Sing xs' -> Tagged xs' f -> [Tagged xs' f]
      go (((singInstance -> SingInstance) :: Sing x') `SCons` _) (Here x) =
        map Here $ withAlwaysS @Arbitrary @f @x' sing shrink x
      go (_ `SCons` sxs) (There xs) = map There $ go sxs xs
      go SNil            t          = noHere t

instance SingI xs => Functor (Tagged xs) where
  fmapSing :: forall a b . (forall x . SingI x => a x -> b x) -> Tagged xs a -> Tagged xs b
  fmapSing fn = go sing
    where
      go :: forall xs' . SList xs' -> Tagged xs' a -> Tagged xs' b
      go ((singInstance -> SingInstance) `SCons` _  ) (Here  x) = Here (fn x)
      go (_                              `SCons` sxs) (There x) = There $ go sxs x
      go SNil x = noHere x

getTaggedValueS :: forall xs f y . SingI xs => (forall x . SingI x => f x -> y) -> Tagged xs f -> y
getTaggedValueS fn = go (sing @_ @xs)
  where
    go :: forall xs' . SList xs' -> Tagged xs' f -> y
    go = \case
      SNil -> noHere
      SCons (singInstance -> SingInstance) sxs -> \case
        Here  x -> fn x
        There x -> go sxs x
