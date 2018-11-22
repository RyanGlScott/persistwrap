{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Embedding.Class.Itemized where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Writer (MonadWriter)
import Data.Singletons.TypeLits (Symbol)

import PersistWrap.Table (MonadDML(..), MonadTransactable)

newtype Itemized (items :: [(Symbol, *)]) m x = Itemized {runItemized :: m x}
  deriving ( Functor, Applicative, Monad, MonadTransactable, MonadIO
           , MonadReader r, MonadWriter w, MonadState s, MonadError e
           )
instance MonadTrans (Itemized items) where
  lift = Itemized
instance MonadDML m => MonadDML (Itemized items m) where
  type Transaction (Itemized items m) = Itemized items (Transaction m)
  atomicTransaction (Itemized act) = Itemized $ atomicTransaction act
