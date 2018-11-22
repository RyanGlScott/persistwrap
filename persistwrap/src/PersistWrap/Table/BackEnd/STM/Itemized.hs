{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.BackEnd.STM.Itemized
    ( withEmptyTablesItemized
    ) where

import Control.Monad.IO.Class (MonadIO)

import Conkin.Extra (Always)
import PersistWrap.Embedding.Class.Itemized (Itemized)
import PersistWrap.Table.BackEnd.Helper (AllEmbed, Items, setupHelper)
import PersistWrap.Table.BackEnd.STM.Internal

withEmptyTablesItemized
  :: forall fnitems m x
   . (MonadIO m, Always AllEmbed fnitems)
  => (forall s . Itemized (Items (fnitems (FK s))) (TVarDMLT s m) x)
  -> m x
withEmptyTablesItemized = setupHelper @fnitems (\schs act -> setupEmptyTables schs (const act))
