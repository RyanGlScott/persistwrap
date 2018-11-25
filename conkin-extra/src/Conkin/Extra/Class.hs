{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Conkin.Extra.Class where

class Always c f where
  withAlways :: forall x y. (c (f x) => y) -> y
