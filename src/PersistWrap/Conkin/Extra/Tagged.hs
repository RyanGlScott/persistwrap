{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra.Tagged
    ( compareAlwaysTags
    , eqAlwaysTags
    , findJust
    , getSingle
    , leftTag
    , noHere
    , pickSide
    , rightTag
    , swapOptions
    ) where

import Conkin (Tagged(..), Tuple(..))
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.Prelude (type (++), SList, Sing(SCons, SNil))

import PersistWrap.Conkin.Extra.Class (Always, (==*), compare1)
import PersistWrap.Conkin.Extra.Tagged.NoHere (noHere)

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

getSingle :: Tagged '[x] f -> f x
getSingle = \case
  Here  x -> x
  There r -> noHere r

swapOptions :: Tagged '[x, y] f -> Tagged '[y, x] f
swapOptions = \case
  Here  x                -> There $ Here x
  There (getSingle -> x) -> Here x

findJust :: forall xs f g . (forall x . f x -> Maybe (g x)) -> Tuple xs f -> Maybe (Tagged xs g)
findJust fn = go
  where
    go :: forall xs' . Tuple xs' f -> Maybe (Tagged xs' g)
    go = \case
      Nil         -> Nothing
      x `Cons` xs -> case fn x of
        Just y  -> Just $ Here y
        Nothing -> There <$> go xs

compareAlwaysTags
  :: forall xs f . (Always Ord f, SingI xs) => Tagged xs f -> Tagged xs f -> Ordering
compareAlwaysTags xs ys = case (sing @_ @xs, xs, ys) of
  ((sx :: Sing x) `SCons` _, Here x, Here y) -> withSingI sx compare1 x y
  (_, Here{}, There{}) -> LT
  (_, There{}, Here{}) -> GT
  (_ `SCons` sxs, There x, There y) -> withSingI sxs compareAlwaysTags x y

eqAlwaysTags
  :: forall xs f . (Always Eq f, SingI xs) => Tagged xs f -> Tagged xs f -> Bool
eqAlwaysTags xs ys = case (sing @_ @xs, xs, ys) of
  ((sx :: Sing x) `SCons` _, Here x, Here y) -> withSingI sx (==*) x y
  (_, Here{}, There{}) -> False
  (_, There{}, Here{}) -> False
  (_ `SCons` sxs, There x, There y) -> withSingI sxs eqAlwaysTags x y
