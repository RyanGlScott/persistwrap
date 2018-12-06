module PersistWrap.Persistable.State
    ( state
    ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import GHC.Stack (HasCallStack)

import PersistWrap.Persistable.Delete (deleteDescendants)
import PersistWrap.Persistable.Get (get)
import PersistWrap.Persistable.Insert (insertRowItems)
import PersistWrap.Persistable.Insert.Utils
import PersistWrap.Persistable.Rep
import PersistWrap.Structure
import PersistWrap.Table as Table

state
  :: forall schemaName structure m b
   . (HasCallStack, MonadTransaction m)
  => NamedSchemaRep (ForeignKey m) schemaName structure
  -> ForeignKey m schemaName
  -> (EntityOf (ForeignKey m) structure -> (b, EntityOf (ForeignKey m) structure))
  -> m (Maybe b)
state nschr fk op = runMaybeT $ do
  x <- MaybeT $ get nschr fk
  let (res, x') = op x
  lift $ replace nschr fk x'
  pure res

replace
  :: forall schemaName structure m
   . (HasCallStack, MonadTransaction m)
  => NamedSchemaRep (ForeignKey m) schemaName structure
  -> ForeignKey m schemaName
  -> EntityOf (ForeignKey m) structure
  -> m ()
replace nschr@(NamedSchemaRep selfSchemaName rep) fk x = do
  deleteDescendants nschr fk
  withPerformReplace selfSchemaName fk (insertRowItems selfSchemaName rep x)
