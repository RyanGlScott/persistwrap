{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PersistWrap.TestUtils.Operations
    ( ConvertF(..)
    , DummyFK
    , ItemList(..)
    , Member(..)
    , MemberX(..)
    , OpSchema(..)
    , Operation(..)
    , Operations(..)
    , dummyToAny
    ) where

import Conkin (Tuple(..))
import qualified Conkin
import Control.Applicative (empty)
import Control.Monad (filterM, forM)
import Control.Monad.State (evalState, evalStateT, lift)
import qualified Control.Monad.State as State
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Set as Set
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude (type (==), Sing(SFalse, STrue), (%==))
import Data.Singletons.TypeLits (SSymbol, Symbol)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Test.QuickCheck hiding (elements)
import Unsafe.Coerce (unsafeCoerce)

import Conkin.Extra (mapUncheck)
import PersistWrap.Persisted (MapsTo)
import PersistWrap.Structure (EntityOf, SStructure, Structure)

data DummyFK (name :: Symbol)
  deriving (Eq,Ord,Show)
instance Arbitrary (DummyFK name) where
  arbitrary = error "Should be unreachable"
  shrink    = \case {}

dummyToAny :: DummyFK name -> fk name
dummyToAny = \case {}

data OpSchema = forall items. ItemList items => OpSchema [Structure Text] (Proxy items)
instance Show OpSchema where
  showsPrec d (OpSchema xs _) = showParen (d > 10) $ showString "makeOpSchema " . showsPrec 11 xs

makeOpSchema :: [Structure Text] -> OpSchema
makeOpSchema = go . zip defaultTabNames
  where
    go :: [(Text, Structure Text)] -> OpSchema
    go = \case
      []              -> OpSchema [] (Proxy @'[])
      (name, x) : nxs -> case (toSing name, toSing x, go nxs) of
        (SomeSing (sname :: SSymbol name), SomeSing ((singInstance -> SingInstance) :: SStructure x), OpSchema xs (_ :: Proxy
            rest))
          -> withSingI sname $ OpSchema (x : xs) $ Proxy @('(name, EntityOf DummyFK x) ': rest)

defaultTabNames :: [Text]
defaultTabNames = [ Text.pack $ "Tab" ++ show i | i <- [(0 :: Int) ..] ]

instance Arbitrary OpSchema where
  arbitrary = makeOpSchema <$> arbitrary
  shrink (OpSchema xs _) = makeOpSchema <$> shrink xs

newtype Operations items = Operations [Operation items]
deriving instance Show (Operations items)

-- | Generates one of the given values. The input list must be non-empty.
elements :: HasCallStack => [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

data Operation items
  = Delete (Member items Proxy) Int
  | Insert Int (Member items Identity)
  | Lookup (Member items Proxy) Int
  | Replace Int (Member items Identity)

instance Show (Operation items) where
  showsPrec d = \case
    Insert i (Member (MemberX name (Identity x))) ->
      showParen (d > 10)
        $ showString "insert "
        . showsPrec 11 x
        . showString " into "
        . showString (Text.unpack (fromSing name))
        . showString " as "
        . showString ("fk" ++ show i)
    Lookup (Member (MemberX name Proxy)) j ->
      showParen (d > 10)
        $ showString "lookup "
        . showString ("fk" ++ show j)
        . showString " in "
        . showString (Text.unpack (fromSing name))
    Delete (Member (MemberX name Proxy)) j ->
      showParen (d > 10)
        $ showString "delete "
        . showString ("fk" ++ show j)
        . showString " in "
        . showString (Text.unpack (fromSing name))
    Replace j (Member (MemberX name (Identity x))) ->
      showParen (d > 10)
        $ showString "replace "
        . showString ("fk" ++ show j)
        . showString " with "
        . showsPrec 11 x
        . showString " in "
        . showString (Text.unpack (fromSing name))

data OperationC = DeleteC | InsertC | LookupC | ReplaceC
  deriving (Eq, Bounded, Enum)

_opConstructor :: Operation items -> OperationC
_opConstructor = \case
  Delete{}  -> DeleteC
  Insert{}  -> InsertC
  Lookup{}  -> LookupC
  Replace{} -> ReplaceC

data MemberX items f nx where
  MemberX
    ::(x ~ EntityOf DummyFK structure, MapsTo name x items, SingI structure, Eq (f x))
    => SSymbol name -> f x -> MemberX items f '(name,x)
data Member items f = forall nx. Member (MemberX items f nx)
instance ConvertF f => Show (Member items f) where
  showsPrec d (Member (MemberX _ x)) = showsPrecOrig d x

class ConvertF (f :: * -> *) where
  showsPrecOrig :: Show x => Int -> f x -> ShowS
instance ConvertF Identity where
  showsPrecOrig d (Identity x) = showsPrec d x

-- FunctionalDependencies annoyingly not recognizing type equality here
useMapsTo
  :: (MapsTo name x1 items, MapsTo name x2 items)
  => proxy0 items
  -> proxy1 name
  -> proxy2 name
  -> f x1
  -> f x2
useMapsTo _ _ _ = unsafeCoerce

instance Eq (Member items f) where
  (==) (Member (MemberX snl xl)) (Member (MemberX snr xr)) = case snl %~ snr of
    Proved Refl -> useMapsTo (Proxy @items) snl snr xl == xr
    Disproved{} -> False

upMember
  :: forall sym x items f nx . SingI sym => MemberX items f nx -> MemberX ('(sym,x) ': items) f nx
upMember (MemberX name x) = case name %== sing @sym of
  STrue  -> error "Duplicate names"
  SFalse -> MemberX name x

class ItemList (items :: [(Symbol,*)]) where
  members :: Tuple items (MemberX items Proxy)
instance ItemList '[] where
  members = Nil
instance (SingI sym, (sym == sym) ~ 'True
          , x ~ EntityOf DummyFK structure, SingI structure
          , ItemList rest)
    => ItemList ('(sym,x) ': rest) where
  members = MemberX (sing @sym) (Proxy @x) `Cons` Conkin.fmap upMember (members @rest)

instance ItemList items => Arbitrary (Operations items) where
  arbitrary = do
    let mems = mapUncheck Member $ members @items
    len <- sized $ \s -> choose (0, s)
    if null mems || len == 0
      then return $ Operations []
      else do
        mem0@(Member (MemberX n0 _)) <- elements mems
        firstInsertion               <- Insert 0 . Member . MemberX n0 <$> arbitrary
        restOps                      <- (`evalStateT` [(0, mem0)]) $ forM [1 .. len] $ \i ->
          lift arbitraryBoundedEnum >>= \case
            DeleteC -> do
              opts   <- State.get
              (j, m) <- lift $ elements opts
              return $ Delete m j
            InsertC -> do
              memi@(Member (MemberX n _)) <- lift $ elements mems
              State.modify ((i, memi) :)
              lift $ Insert i . Member . MemberX n <$> arbitrary
            LookupC -> do
              opts   <- State.get
              (j, m) <- lift $ elements opts
              return $ Lookup m j
            ReplaceC -> do
              opts <- State.get
              (j, Member (MemberX sn (_ :: Proxy x))) <- lift $ elements opts
              Replace j . Member . MemberX sn . Identity <$> lift (arbitrary @x)
        return $ Operations $ firstInsertion : restOps
  shrink (Operations xs) = Operations . removeOrphanedLookups <$> shrinkList shrinkOp xs

removeOrphanedLookups :: [Operation items] -> [Operation items]
removeOrphanedLookups ops = (`evalState` Set.empty) $ (`filterM` ops) $ \case
  Delete  _ j -> State.gets (j `Set.member`)
  Insert  i _ -> State.modify (Set.insert i) >> return True
  Lookup  _ j -> State.gets (j `Set.member`)
  Replace j _ -> State.gets (j `Set.member`)

shrinkOp :: Operation items -> [Operation items]
shrinkOp = \case
  Delete{}                         -> empty
  Insert i (Member (MemberX n x))  -> Insert i . Member . MemberX n <$> shrink x
  Lookup{}                         -> empty
  Replace j (Member (MemberX n x)) -> Replace j . Member . MemberX n <$> shrink x
