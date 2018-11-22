module StreamReader
    ( StreamReader
    , StreamReaderT
    , ask
    , runStreamReader
    , runStreamReaderT
    , splitStreamReader
    ) where

import Control.Monad.Morph (MFunctor)
import Control.Monad.State (StateT, lift, runStateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadTrans)
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Stack (HasCallStack)

newtype StreamReaderT a m x = StreamReaderT (StateT [a] m x)
  deriving (Functor, Applicative, Monad, MonadTrans, MFunctor)
type StreamReader a = StreamReaderT a Identity

ask :: (HasCallStack, Monad m) => StreamReaderT a m a
ask = StreamReaderT $ State.state $ \case
  []       -> error "Out of values"
  (x : xs) -> (x, xs)

runStreamReaderT :: (HasCallStack, Monad m) => StreamReaderT a m x -> [a] -> m x
runStreamReaderT (StreamReaderT act) xs = do
  (res, remainder) <- runStateT act xs
  case remainder of
    []    -> return res
    _ : _ -> error "More values remaining"

runStreamReader :: HasCallStack => StreamReader a x -> [a] -> x
runStreamReader act = runIdentity . runStreamReaderT act

splitStreamReader :: (HasCallStack, Monad m) => StreamReaderT a m x -> StreamReaderT a m x
splitStreamReader act = StreamReaderT $ lift . runStreamReaderT act =<< State.get
