module PersistWrap.Maybe.Extra
    ( fromJust
    ) where

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

fromJust :: HasCallStack => Maybe a -> a
fromJust = fromMaybe (error "*** Exception: Maybe.fromJust: Nothing")
