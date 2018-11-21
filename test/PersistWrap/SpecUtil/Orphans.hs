{-# OPTIONS_GHC -Wno-orphans #-}

module PersistWrap.SpecUtil.Orphans () where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Generics.Deriving.Eq (GEq(..))
import Generics.Deriving.Show (GShow(..))

instance GEq ByteString where geq = (==)
instance GEq Text where geq = (==)

instance GShow ByteString where gshowsPrec = showsPrec
instance GShow Text where gshowsPrec = showsPrec
