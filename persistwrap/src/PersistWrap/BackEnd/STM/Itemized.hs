{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.BackEnd.STM.Itemized
    ( withEmptyTablesItemized
    ) where

import Control.Monad.IO.Class (MonadIO)
import GHC.Stack (HasCallStack)

import Conkin.Extra (Always)
import PersistWrap.BackEnd.Helper (AllEmbed, Items, setupHelper)
import PersistWrap.Itemized (Itemized)
import PersistWrap.Table (ForeignKey)
import PersistWrap.Table.BackEnd.STM.Internal (STMPersist, unsafeSetupEmptyTables)

withEmptyTablesItemized
  :: forall fnitems m x
   . (HasCallStack, MonadIO m, Always AllEmbed fnitems)
  => (forall s . Itemized (Items (fnitems (ForeignKey (STMPersist s m)))) (STMPersist s m) x)
  -> m x
withEmptyTablesItemized =
  setupHelper @fnitems (\schs act -> unsafeSetupEmptyTables schs (const act))
