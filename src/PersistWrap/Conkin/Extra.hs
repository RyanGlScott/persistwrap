{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra
  ( mapUncheck
  , htraverse
  , module X
  ) where

import Conkin (Dispose(..), getFlip)
import qualified Conkin
import qualified Data.DList as DList
import Data.Functor.Compose (getCompose)

import PersistWrap.Conkin.Extra.Class as X
import PersistWrap.Conkin.Extra.Some as X
import PersistWrap.Conkin.Extra.Tagged as X
import PersistWrap.Conkin.Extra.Tuple as X

mapUncheck :: (Conkin.Foldable f) => (forall x . a x -> y) -> f a -> [y]
mapUncheck fn = DList.toList . Conkin.foldMap (DList.singleton . fn)

htraverse
  :: (Prelude.Applicative f, Conkin.Traversable t) => (forall x . a x -> f (b x)) -> t a -> f (t b)
htraverse fn t = Conkin.fmap (unComposeConst . getFlip) . getCompose <$> getDispose
  (Conkin.traverse (Dispose . fmap ComposeConst . fn) t)

newtype ComposeConst a b c = ComposeConst {unComposeConst :: a b}
