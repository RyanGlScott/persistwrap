{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}

module Conkin.Extra.Tagged
    ( findJust
    , getSingle
    , noHere
    , swapOptions
    ) where

import Conkin (Tagged(..), Tuple(..))

import Conkin.Extra.Tagged.NoHere (noHere)

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
