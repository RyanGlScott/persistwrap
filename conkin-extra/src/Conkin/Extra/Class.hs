{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Conkin.Extra.Class where

class (forall x. c (f x)) => Always c f where
instance (forall x. c (f x)) => Always c f where
