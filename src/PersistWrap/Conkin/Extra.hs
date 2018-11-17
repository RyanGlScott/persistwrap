{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra
  ( (:*:)(..)
  , module X
  ) where

import GHC.Generics ((:*:)(..))

import PersistWrap.Conkin.Extra.Class as X
import PersistWrap.Conkin.Extra.Some as X
import PersistWrap.Conkin.Extra.Tagged as X
import PersistWrap.Conkin.Extra.Traversal as X
import PersistWrap.Conkin.Extra.Tuple as X
