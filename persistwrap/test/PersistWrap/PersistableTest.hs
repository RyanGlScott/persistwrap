{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.PersistableTest
    ( prop_insert_lookup
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
import Test.QuickCheck

import Conkin.Extra (All, Always(..))
import Consin (AlwaysS(..), Some(Some))
import PersistWrap hiding (fmapFK)
import PersistWrap.Functor.Extra ((<&>))
import qualified PersistWrap.Structure as Structure (fmapFK)
import PersistWrap.BackEnd.Helper (AllEmbed, EmbedPair)
import qualified PersistWrap.BackEnd.STM.Itemized as BackEnd
import qualified PersistWrap.Persistable as Persistable
import PersistWrap.Table.BackEnd.STM (STMTransaction)
import qualified PersistWrap.Table.BackEnd.STM as BackEnd
import PersistWrap.TestUtils.Operations

prop_insert_lookup :: Property
prop_insert_lookup = withMaxSuccess 1000 $ property $ \(OpSchema _ (_ :: Proxy items)) ->
  property $ \(ops :: Operations items) -> ioProperty $ do
    actualRes <- operateActual ops
    return $ actualRes === operateModel ops

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

anyToUnit :: fk (name :: Symbol) -> Proxy name
anyToUnit = const Proxy

convertFK :: Member items Identity -> Member items (ConvertFK Proxy)
convertFK (Member (MemberX name (Identity val))) =
  Member $ MemberX name (ConvertFK $ Structure.fmapFK anyToUnit val)

type LookupResults items = [OpResult items]

data OpResult items
  = Done
  | Found (Member items (ConvertFK Proxy))
  | NotFound
  deriving (Eq, Show)

operateModel :: Operations xs -> LookupResults xs
operateModel (Operations ops) = evalState (mapM go ops) IntMap.empty
  where
    go :: Operation xs -> State (IntMap (Member xs Identity)) (OpResult xs)
    go = \case
      Delete _ i -> State.gets (IntMap.lookup i) >>= \case
        Nothing -> pure NotFound
        Just{}  -> State.modify (IntMap.delete i) >> pure Done
      Insert i mem -> do
        State.modify (IntMap.insert i mem)
        pure Done
      Lookup _ i -> State.gets (IntMap.lookup i) <&> \case
        Nothing -> NotFound
        Just r  -> Found $ convertFK r

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
            Delete (Member (MemberX _ (_ :: Proxy x))) j -> do
              Some fkj <- State.gets (IntMap.! j)
              result   <-
                Persistable.deleteX @_ @(ConvertFK (ForeignKey (BackEnd.STMTransaction s)) x) fkj
              return $ if result then Done else NotFound
            Insert i (Member (MemberX ((singInstance -> SingInstance) :: SSymbol name) (Identity x)))
              -> do
                fki <- Persistable.insertX @name
                  (Structure.fmapFK
                    (dummyToAny :: forall n . DummyFK n -> ForeignKey (BackEnd.STMTransaction s) n)
                    x
                  )
                State.modify $ IntMap.insert i (Some fki)
                return Done
            Lookup (Member (MemberX name (_ :: Proxy x))) j -> do
              Some fkj <- State.gets (IntMap.! j)
              ent <- Persistable.getX @_ @(ConvertFK (ForeignKey (BackEnd.STMTransaction s)) x) fkj
              return $ case ent of
                Nothing -> NotFound
                Just (ConvertFK x) ->
                  Found $ Member $ MemberX name $ ConvertFK $ Structure.fmapFK anyToUnit x
