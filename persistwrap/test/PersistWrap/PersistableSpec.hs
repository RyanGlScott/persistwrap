{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.PersistableSpec
    ( spec
    ) where

import Conkin (Tuple(..))
import Control.Monad (forM)
import qualified Control.Monad.State as State
import Control.Monad.State (State, evalState, evalStateT)
import Data.Functor.Identity
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Singletons
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
    $ \(OpSchema _ (_ :: Proxy items)) -> property $ \(ops :: Operations items) -> ioProperty $ do
        actualRes <- operateActual ops
        return $ actualRes === operateModel ops

data UnitFK (name :: Symbol) = UnitFK
  deriving (Eq, Ord, Show)
instance AlwaysS Eq UnitFK where withAlwaysS = const id
instance AlwaysS Ord UnitFK where withAlwaysS = const id
instance AlwaysS Show UnitFK where withAlwaysS = const id

anyToUnit :: fk name -> UnitFK name
anyToUnit = const UnitFK

data ConvertFK fk2 x where
  ConvertFK :: SingI x => EntityOf fk2 x -> ConvertFK fk2 (EntityOf fk1 x)
deriving instance AlwaysS Eq fk2 => Eq (ConvertFK fk2 x)
deriving instance AlwaysS Show fk2 => Show (ConvertFK fk2 x)
instance (x ~ EntityOf fk1 struct, EntityPart fk1 x)
    => EntityPart fk2 (ConvertFK fk2 (EntityOf fk1 struct)) where
  type StructureOf (ConvertFK fk2 (EntityOf fk1 struct)) = struct
  fromEntity = ConvertFK
  toEntity (ConvertFK x) = x

instance AlwaysS Show fk2 => ConvertF (ConvertFK fk2) where
  showsPrecOrig d (ConvertFK x) = showsPrec d x

convertFK :: Member items Identity -> Member items (ConvertFK UnitFK)
convertFK (Member (MemberX name (Identity val))) =
  Member $ MemberX name (ConvertFK $ Structure.fmapFK anyToUnit val)

type LookupResults items = [Maybe (Member items (ConvertFK UnitFK))]

operateModel :: Operations xs -> LookupResults xs
operateModel (Operations ops) = evalState (mapM go ops) IntMap.empty
  where
    go :: Operation xs -> State (IntMap (Member xs Identity)) (Maybe (Member xs (ConvertFK UnitFK)))
    go = \case
      Insert i mem -> do
        State.modify (IntMap.insert i mem)
        pure Nothing
      Lookup _ i -> do
        x <- State.gets (IntMap.lookup i)
        pure $ convertFK <$> x

data General (xs :: [(Symbol, *)]) (fk :: Symbol -> *)
type instance Items (General xs fk) = Entities xs fk
type family Entities (xs :: [(Symbol, *)]) (fk :: Symbol -> *) where
  Entities '[] fk = '[]
  Entities ('(k,v) ': kvs) fk = '(k, ConvertFK fk v) ': Entities kvs fk
instance ItemList kvs => Always AllEmbed (General kvs) where
  withAlways :: forall proxy fk y . proxy fk -> (AllEmbed (General kvs fk) => y) -> y
  withAlways _ y0 = go y0 members
    where
      go
        :: forall kvs'
         . (All EmbedPair (Entities kvs' fk) => y)
        -> Tuple kvs' (MemberX kvs Proxy)
        -> y
      go y = \case
        Nil -> y
        MemberX (singInstance -> SingInstance) _ `Cons` xs -> go y xs

operateActual :: forall xs . ItemList xs => Operations xs -> IO (LookupResults xs)
operateActual (Operations ops) = BackEnd.withEmptyTablesItemized @(General xs)
  $ atomicTransaction go
  where
    go :: forall s . ItemizedIn (General xs) (STMTransaction s) (LookupResults xs)
    go =
      (`evalStateT` (IntMap.empty :: IntMap (Some (ForeignKey (STMTransaction s)))))
        $ forM ops
        $ \case
            Insert i (Member (MemberX ((singInstance -> SingInstance) :: SSymbol name) (Identity x)))
              -> do
                fki <- Persistable.insertX @name
                  (Structure.fmapFK
                    (dummyToAny :: forall n . DummyFK n -> ForeignKey (BackEnd.STMTransaction s) n)
                    x
                  )
                State.modify $ IntMap.insert i (Some fki)
                return Nothing
            Lookup (Member (MemberX name (_ :: Proxy x))) j -> do
              Some fkj <- State.gets (IntMap.! j)
              ent <- Persistable.getX @_ @(ConvertFK (ForeignKey (BackEnd.STMTransaction s)) x) fkj
              return
                $   (\(ConvertFK x) ->
                      Member $ MemberX name $ ConvertFK $ Structure.fmapFK anyToUnit x
                    )
                <$> ent
