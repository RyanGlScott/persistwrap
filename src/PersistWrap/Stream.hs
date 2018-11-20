module PersistWrap.Stream
    ( Stream
    , StreamT
    , get
    , runStream
    , runStreamT
    , splitStream
    ) where

import Control.Monad.Morph (MFunctor)
import Control.Monad.State (StateT, lift, runStateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadTrans)
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Stack (HasCallStack)

newtype StreamT a m x = StreamT (StateT [a] m x)
  deriving (Functor, Applicative, Monad, MonadTrans, MFunctor)
type Stream a = StreamT a Identity

get :: (HasCallStack, Monad m) => StreamT a m a
get = StreamT $ State.state $ \case
  []       -> error "Out of values"
  (x : xs) -> (x, xs)

runStreamT :: (HasCallStack, Monad m) => StreamT a m x -> [a] -> m x
runStreamT (StreamT act) xs = do
  (res, remainder) <- runStateT act xs
  case remainder of
    []    -> return res
    _ : _ -> error "More values remaining"

runStream :: HasCallStack => Stream a x -> [a] -> x
runStream act = runIdentity . runStreamT act

splitStream :: (HasCallStack, Monad m) => StreamT a m x -> StreamT a m x
splitStream act = StreamT $ lift . runStreamT act =<< State.get
