{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra.Tuple where

import Prelude hiding (unzip)

import Conkin (Tuple(..))
import Data.Constraint (Dict(Dict))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Singletons (Sing, SingI, sing, withSingI)
import Data.Singletons.Prelude (type (++), SList, Sing(SCons, SNil))
import GHC.Generics ((:*:)((:*:)))

import qualified PersistWrap.Conkin.Extra.Class as AlwaysS (AlwaysS(..))
import PersistWrap.Conkin.Extra.Class (AlwaysS, (==*), compare1)
import PersistWrap.Conkin.Extra.Traversal (mapUncheck)

splitTuple :: forall xs ys f . SingI xs => Tuple (xs ++ ys) f -> (Tuple xs f, Tuple ys f)
splitTuple t = case sing @_ @xs of
  SNil                       -> (Nil, t)
  SCons _ (xs' :: SList xs') -> case t of
    (v `Cons` vs) -> case withSingI xs' (splitTuple @xs' @ys) vs of
      (l, r) -> (v `Cons` l, r)

(++&) :: Tuple xs f -> Tuple ys f -> Tuple (xs ++ ys) f
(++&) Nil         t  = t
(++&) (Cons x xs) ys = Cons x (xs ++& ys)

singToTuple :: SList xs -> Tuple xs Sing
singToTuple = \case
  SNil       -> Nil
  SCons x xs -> Cons x (singToTuple xs)

tupleToSing :: Tuple xs Sing -> SList xs
tupleToSing = \case
  Nil       -> SNil
  Cons x xs -> SCons x (tupleToSing xs)

fmapSing
  :: forall a b xs . SingI xs => (forall x . SingI x => a x -> b x) -> Tuple xs a -> Tuple xs b
fmapSing fn = go sing
  where
    go :: forall xs' . SList xs' -> Tuple xs' a -> Tuple xs' b
    go SNil             Nil           = Nil
    go (sx `SCons` sxs) (x `Cons` xs) = withSingI sx fn x `Cons` go sxs xs

zipWithSing
  :: forall a b c xs
   . SingI xs
  => (forall x . SingI x => a x -> b x -> c x)
  -> Tuple xs a
  -> Tuple xs b
  -> Tuple xs c
zipWithSing fn = go sing
  where
    go :: forall xs' . SList xs' -> Tuple xs' a -> Tuple xs' b -> Tuple xs' c
    go SNil             Nil           Nil           = Nil
    go (sx `SCons` sxs) (x `Cons` xs) (y `Cons` ys) = withSingI sx fn x y `Cons` go sxs xs ys

mapUncheckSing :: forall a xs y . SList xs -> (forall x . SingI x => a x -> y) -> Tuple xs a -> [y]
mapUncheckSing s fn = go s
  where
    go :: forall xs' . SList xs' -> Tuple xs' a -> [y]
    go SNil             Nil           = []
    go (sx `SCons` sxs) (x `Cons` xs) = withSingI sx fn x : go sxs xs

mapUncheckNonEmpty :: forall a x xs y . (forall x' . a x' -> y) -> Tuple (x ': xs) a -> NonEmpty y
mapUncheckNonEmpty fn (x0 `Cons` xs0) = fn x0 :| mapUncheck fn xs0

zipUncheck :: forall a b xs y . (forall x . a x -> b x -> y) -> Tuple xs a -> Tuple xs b -> [y]
zipUncheck fn = go
  where
    go :: forall xs' . Tuple xs' a -> Tuple xs' b -> [y]
    go Nil           Nil           = []
    go (x `Cons` xs) (y `Cons` ys) = fn x y : go xs ys

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
    go SNil             Nil           Nil           = []
    go (sx `SCons` sxs) (x `Cons` xs) (y `Cons` ys) = withSingI sx fn x y : go sxs xs ys

tail :: Tuple (x ': xs) f -> Tuple xs f
tail (_ `Cons` xs) = xs

compareAlwaysSTuples :: (SingI xs, AlwaysS Ord f) => Tuple xs f -> Tuple xs f -> Ordering
compareAlwaysSTuples x y = fromMaybe EQ $ find (/= EQ) $ zipUncheckSing compare1 x y

eqAlwaysSTuples :: (SingI xs, AlwaysS Eq f) => Tuple xs f -> Tuple xs f -> Bool
eqAlwaysSTuples x y = and $ zipUncheckSing (==*) x y

withAlwaysSShow :: forall xs f y . (SingI xs, AlwaysS Show f) => (Show (Tuple xs f) => y) -> y
withAlwaysSShow cont = case buildShowDict (sing @_ @xs) of
  Dict -> cont
  where
    buildShowDict :: forall xs' . SList xs' -> Dict (Show (Tuple xs' f))
    buildShowDict = \case
      SNil -> Dict
      ((x :: Sing x) `SCons` xs) ->
        case (withSingI x $ AlwaysS.dictS @Show @f @x, buildShowDict xs) of
          (Dict, Dict) -> Dict

unzip :: Tuple xs (f :*: g) -> (Tuple xs f, Tuple xs g)
unzip = \case
  Nil                  -> (Nil, Nil)
  (x :*: y `Cons` xys) -> let (xs, ys) = unzip xys in (x `Cons` xs, y `Cons` ys)
