{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra.Tagged where

import Conkin (Tagged (..))
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.Prelude (type (++), SList, Sing (SCons, SNil))

import PersistWrap.Conkin.Extra.Class (Always, (==*), compare1)

pickSide
  :: forall (xs :: [k]) (ys :: [k]) (f :: k -> *)
   . SingI xs
  => Tagged (xs ++ ys) f
  -> Either (Tagged xs f) (Tagged ys f)
pickSide l = case (sing :: SList xs) of
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
rightTag = case (sing :: SList xs) of
  SNil                       -> id
  SCons _ (xs' :: SList xs') -> There . withSingI xs' (rightTag @xs' @ys)

newtype AlwaysTagged xs f = AT (Tagged xs f)

instance (Always Eq f, SingI xs) => Eq (AlwaysTagged xs f) where
  (==) (AT x0) (AT y0) = go (sing :: Sing xs) x0 y0
    where
      go :: forall xs'. Sing xs' -> Tagged xs' f -> Tagged xs' f -> Bool
      go SNil x _ = case x of {}
      go (sx `SCons` _) (Here x) (Here y) = withSingI sx (==*) x y
      go (_ `SCons` sxs) (There x) (There y) = go sxs x y
      go (_ `SCons` _) (Here _) (There _) = False
      go (_ `SCons` _) (There _) (Here _) = False

instance (Always Eq f, Always Ord f, SingI xs) => Ord (AlwaysTagged xs f) where
  compare (AT x0) (AT y0) = go (sing :: Sing xs) x0 y0
    where
      go :: forall xs'. Sing xs' -> Tagged xs' f -> Tagged xs' f -> Ordering
      go SNil x _ = case x of {}
      go (sx `SCons` _) (Here x) (Here y) = withSingI sx compare1 x y
      go (_ `SCons` sxs) (There x) (There y) = go sxs x y
      go (_ `SCons` _) (Here _) (There _) = LT
      go (_ `SCons` _) (There _) (Here _) = GT
