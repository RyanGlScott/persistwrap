{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Conkin.Extra
  ( (:*:)(..)
  , module X
  ) where

import GHC.Generics ((:*:)(..))

import Conkin.Extra.All as X
import Conkin.Extra.Class as X
import Conkin.Extra.Index as X
import Conkin.Extra.Tagged as X
import Conkin.Extra.Traversal as X
import Conkin.Extra.Tuple as X
