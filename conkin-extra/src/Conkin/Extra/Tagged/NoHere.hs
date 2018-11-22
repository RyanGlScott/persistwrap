{-# LANGUAGE PolyKinds #-}

module Conkin.Extra.Tagged.NoHere
    ( noHere
    ) where

import Conkin (Tagged(..))

noHere :: Tagged '[] f -> a
noHere = \case {}
