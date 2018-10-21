{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra where

import Conkin (Dispose (..), Tagged (..), Tuple (..), getFlip)
import qualified Conkin
import Data.Functor.Compose (getCompose)
import Data.Singletons.Prelude

pickSide
  :: forall (xs :: [k]) (ys :: [k]) (f :: k -> *)
   . SingI xs
  => Tagged (xs ++ ys) f
  -> Either (Tagged xs f) (Tagged ys f)
pickSide l = case (sing :: SList xs) of
  SNil        -> Right l
  SCons _ xs' -> case l of
    Here  x    -> Left $ Here x
    There rest -> case withSingI xs' $ pickSide rest of
      Left  lres -> Left $ There lres
      Right rres -> Right rres

leftTag :: forall xs ys f . Tagged xs f -> Tagged (xs ++ ys) f
leftTag = \case
  Here  x                   -> Here x
  There (x :: Tagged xs' f) -> There $ leftTag @xs' @ys x

rightTag :: forall xs ys f . SingI xs => Tagged ys f -> Tagged (xs ++ ys) f
rightTag = case (sing :: SList xs) of
  SNil                      -> id
  SCons _ (xs' :: SList xs') -> There . withSingI xs' (rightTag @xs' @ys)

splitTuple :: forall xs ys f . SingI xs => Tuple (xs ++ ys) f -> (Tuple xs f, Tuple ys f)
splitTuple t = case (sing :: SList xs) of
  SNil                      -> (Nil, t)
  SCons _ (xs' :: SList xs') -> case t of
    (v `Cons` vs) -> case withSingI xs' $ splitTuple @xs' @ys vs of
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

traverseInPrelude
  :: (Prelude.Applicative f, Conkin.Traversable t) => (forall x . a x -> f (b x)) -> t a -> f (t b)
traverseInPrelude fn t = Conkin.fmap (unComposeConst . getFlip) . getCompose <$> getDispose
  (Conkin.traverse (Dispose . fmap ComposeConst . fn) t)

newtype ComposeConst a b c = ComposeConst {unComposeConst :: a b}

zipWith :: forall a b c xs. (forall x. a x -> b x -> c x) -> Tuple xs a -> Tuple xs b -> Tuple xs c
zipWith fn = go
  where
    go :: forall xs'. Tuple xs' a -> Tuple xs' b -> Tuple xs' c
    go Nil Nil = Nil
    go (x `Cons` xs) (y `Cons` ys) = fn x y `Cons` go xs ys

zipUncheck :: forall a b xs y. (forall x. a x -> b x -> y) -> Tuple xs a -> Tuple xs b -> [y]
zipUncheck fn = go
  where
    go :: forall xs'. Tuple xs' a -> Tuple xs' b -> [y]
    go Nil Nil = []
    go (x `Cons` xs) (y `Cons` ys) = fn x y : go xs ys
