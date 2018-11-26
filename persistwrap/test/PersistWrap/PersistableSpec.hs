{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.PersistableSpec
    ( spec
    ) where

import Control.Monad (forM)
import qualified Control.Monad.State as State
import Control.Monad.State (State, evalState, evalStateT)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude (SList, Sing(SCons, SNil, STuple2))
import Data.Singletons.TypeLits (SSymbol, Symbol)
import Test.Hspec
import Test.QuickCheck

import Conkin.Extra (All, Always(..))
import Consin (AlwaysS(..), Some(Some))
import PersistWrap hiding (fmapFK)
import qualified PersistWrap.Structure as Structure (fmapFK)
import PersistWrap.BackEnd.Helper (AllEmbed, EmbedPair)
import qualified PersistWrap.BackEnd.STM.Itemized as BackEnd
import qualified PersistWrap.Persistable as Persistable
import PersistWrap.Table.BackEnd.STM (STMTransaction)
import qualified PersistWrap.Table.BackEnd.STM as BackEnd
import PersistWrap.SpecUtil.Operations

spec :: Spec
spec =
  describe "Insertion and lookup"
    $ it "should behave like the model"
    $ withMaxSuccess 1000
    $ property
    $ \ops -> ioProperty $ do
        actualRes <- operateActual ops
        return $ actualRes === operateModel ops

data UnitFK (name :: Symbol) = UnitFK
  deriving (Eq, Ord, Show)
instance AlwaysS Eq UnitFK where withAlwaysS = const id
instance AlwaysS Ord UnitFK where withAlwaysS = const id
instance AlwaysS Show UnitFK where withAlwaysS = const id

anyToUnit :: fk name -> UnitFK name
anyToUnit = const UnitFK

type LookupResults = [(Int, Maybe (Value UnitFK))]

data Value fk = forall struct. SingI struct => Value (EntityOf fk struct)
instance AlwaysS Eq fk => Eq (Value fk) where
  (==) (Value (lent :: EntityOf fk sl)) (Value (rent :: EntityOf fk sr)) =
    case sing @_ @sl %~ sing @_ @sr of
      Proved Refl -> lent == rent
      Disproved{} -> False
deriving instance AlwaysS Show fk => Show (Value fk)

fmapFK
  :: (AlwaysS Eq fk2, AlwaysS Ord fk2)
  => (forall name . fk1 name -> fk2 name)
  -> Value fk1
  -> Value fk2
fmapFK fn (Value ent) = Value (Structure.fmapFK fn ent)

operateModel :: Operations -> LookupResults
operateModel (Operations _ ops) = evalState (mapM go ops) IntMap.empty
  where
    go :: Operation -> State (IntMap (Value DummyFK)) (Int, Maybe (Value UnitFK))
    go = \case
      Insert (singInstance -> SingInstance) i _ val -> do
        State.modify (IntMap.insert i (Value val))
        pure (i, Nothing)
      Lookup i _ _ -> do
        x <- State.gets (IntMap.lookup i)
        pure (i, fmapFK dummyToAny <$> x)

data General (kvs :: [(Symbol, Structure Symbol)]) (fk :: Symbol -> *)
type instance Items (General kvs fk) = Entities kvs fk
type family Entities kvs fk where
  Entities '[] fk = '[]
  Entities ('(k,v) ': kvs) fk = '(k, EntityOf fk v) ': Items (General kvs fk)
instance SingI kvs => Always AllEmbed (General kvs) where
  withAlways :: forall proxy fk y. proxy fk -> (AllEmbed (General kvs fk) => y) -> y
  withAlways _ y0 = go y0 (sing @_ @kvs)
    where
      go :: forall kvs'. (All EmbedPair (Entities kvs' fk) => y) -> SList kvs' -> y
      go y = \case
        SNil -> y
        STuple2 (singInstance -> SingInstance) (singInstance -> SingInstance) `SCons` xs -> go y xs

operateActual :: Operations -> IO LookupResults
operateActual (Operations (toSing . Map.toList -> (SomeSing (kvs :: SList kvs))) ops) =
  withSingI kvs $ BackEnd.withEmptyTablesItemized @(General kvs) $ atomicTransaction go
  where
    go :: forall s . ItemizedIn (General kvs) (STMTransaction s) LookupResults
    go = (`evalStateT` IntMap.empty) $ forM ops $ \case
      Insert (singInstance -> SingInstance) i (toSing -> (SomeSing (sname :: SSymbol name))) x ->
        withSingI sname $ do
          fki <- Persistable.insertX @name
            (Structure.fmapFK
              (dummyToAny :: forall n . DummyFK n -> ForeignKey (BackEnd.STMTransaction s) n)
              x
            )
          State.modify (IntMap.insert i (Some fki))
          return (i, Nothing)
      Lookup i _ (toSing -> SomeSing ((singInstance -> SingInstance) :: SStructure structure)) -> do
        Some fkj <- State.gets (IntMap.! i)
        ent      <- Persistable.getX @_ @(EntityOf (ForeignKey (STMTransaction s)) structure) fkj
        return (i, fmapFK anyToUnit . Value <$> ent)
