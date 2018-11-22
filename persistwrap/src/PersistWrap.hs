module PersistWrap
    ( PersistTrans
    , EnumVal(..)
    , ForeignKey
    , ForeignKeysShowable
    , ItemizedIn
    , Items
    , MonadPersist(..)
    , MonadTransaction
    , enum
    , module X
    ) where

import Consin (AlwaysS)
import GHC.TypeLits (Symbol)

import PersistWrap.Itemized (Itemized)
import PersistWrap.Persisted as X
import PersistWrap.Structure as X
import PersistWrap.Table

-- For defunctionalization; `x` is a dummy type which can take @ fk :: Symbol -> * @ as a parameter.
type family Items x :: [(Symbol,*)]

type ForeignKeysShowable m = AlwaysS Show (ForeignKey m)

type ItemizedIn ctxt m = Itemized (Items (ctxt (ForeignKey m))) m

type PersistTrans name x m = Persisted name x (Transaction m)
