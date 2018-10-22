{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra.Traverse
    ( traverseInPrelude
    ) where

import Conkin (Dispose (..), getFlip)
import qualified Conkin
import Data.Functor.Compose (getCompose)

traverseInPrelude
  :: (Prelude.Applicative f, Conkin.Traversable t) => (forall x . a x -> f (b x)) -> t a -> f (t b)
traverseInPrelude fn t = Conkin.fmap (unComposeConst . getFlip) . getCompose <$> getDispose
  (Conkin.traverse (Dispose . fmap ComposeConst . fn) t)

newtype ComposeConst a b c = ComposeConst {unComposeConst :: a b}
