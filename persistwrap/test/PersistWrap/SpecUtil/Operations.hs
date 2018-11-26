{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.SpecUtil.Operations
    ( DummyFK
    , Operation(..)
    , Operations(..)
    , SchemaMap
    , dummyToAny
    ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.State (StateT, evalStateT, lift, runStateT)
import qualified Control.Monad.State as State
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Singletons
import Data.Singletons.Decide (Decision(..), (:~:)(Refl), (%~))
import Data.Singletons.TypeLits (Symbol)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Test.QuickCheck hiding (elements)

import Consin (AlwaysS, showsPrec1)
import qualified Consin
import PersistWrap.SpecUtil.StructureShrink
import PersistWrap.Structure (EntityOf, SStructure, Structure)

data DummyFK (name :: Symbol)
  deriving (Eq,Ord,Show)
instance Arbitrary (DummyFK name) where
  arbitrary = discard
  shrink = \case {}
instance AlwaysS Eq DummyFK where withAlwaysS = const id
instance AlwaysS Ord DummyFK where withAlwaysS = const id
instance AlwaysS Show DummyFK where withAlwaysS = const id
instance AlwaysS Arbitrary DummyFK where withAlwaysS = const id

dummyToAny :: DummyFK name -> fk name
dummyToAny = \case {}

type SchemaMap = Map Text (Structure Text)
data Operations = Operations SchemaMap [Operation]
  deriving (Show)

-- | Generates one of the given values. The input list must be non-empty.
elements :: HasCallStack => [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

genInsertion :: SchemaMap -> Int -> Gen (Structure Text, Operation)
genInsertion m i = do
  (k, sv) <- elements $ Map.toList m
  withSomeSing sv $ \(s@(singInstance -> SingInstance) :: SStructure structure) -> do
    v :: EntityOf DummyFK structure <- arbitrary
    return (fromSing s, Insert s i k v)

mapStateTOneof :: [StateT s Gen a] -> StateT s Gen a
mapStateTOneof opts = do
  s         <- State.get
  (x, newS) <- lift $ oneof $ map (`runStateT` s) opts
  State.put newS
  return x

instance Arbitrary Operations where
  arbitrary = do
    sm <-
      Map.fromList
      .   zip [ Text.pack ("tab" ++ show n) | n <- [(0 :: Int) ..] ]
      <$> listOf1 arbitrary
    xsLen <- sized $ \n -> choose (0, n)
    Operations sm <$> if xsLen == 0
      then return []
      else do
        (s0, i0) <- genInsertion sm 0
        rest     <- (`evalStateT` [(0, s0)]) $ forM [1 .. xsLen] $ \i -> mapStateTOneof
          [ do
            (s, res) <- lift (genInsertion sm i)
            State.modify ((i, s) :)
            return res
          , do
            insertionInds <- State.get
            (choice, s)   <- lift $ elements insertionInds
            return $ Lookup choice s
          ]
        return $ i0 : rest
  shrink (Operations m ops) = do
    (k, toSing -> SomeSing v) <- Map.toList m
    (do
        let (ls, rs) = partitionEithers (map (deleteIf k) ops)
        return (Operations (Map.delete k m) (filter (not . looksUpIn ls) rs))
      )
      <|> (do
            StructureShrink v' fn <- buildShrinks v
            let (ls, rs) = partitionEithers (map (updateIf v v' k fn) ops)
            return $ Operations (Map.insert k (fromSing v') m) (filter (not . looksUpIn ls) rs)
          )

looksUpIn :: [(Int, a)] -> Operation -> Bool
looksUpIn xs = \case
  Insert{}   -> False
  Lookup i _ -> i `elem` map fst xs

updateIf
  :: forall s1 s2
   . SStructure s1
  -> SStructure s2
  -> Text
  -> (EntityOf DummyFK s1 -> Maybe (EntityOf DummyFK s2))
  -> Operation
  -> Either (Int, Structure Text) Operation
updateIf s1 s2 key fn op = case op of
  Insert s i name ent -> if key == name
    then case s %~ s1 of
      Proved Refl -> case fn ent of
        Nothing -> Left (i, fromSing s)
        Just r  -> Right $ Insert s2 i name r
      Disproved{} -> error "Types don't match"
    else Right op
  Lookup{} -> Right op

deleteIf :: Text -> Operation -> Either (Int, Structure Text) Operation
deleteIf key op = case op of
  Insert s i name _ -> if key == name then Left (i, fromSing s) else Right op
  Lookup{}          -> Right op

data Operation where
  Insert :: SStructure structure -> Int -> Text -> EntityOf DummyFK structure -> Operation
  Lookup :: Int -> Structure Text -> Operation

instance Show Operation where
  showsPrec d = showParen (d > 10) . \case
    Insert s i n x ->
      showString "Insert "
        . showsPrec 11 s
        . showString " "
        . showsPrec 11 i
        . showString " "
        . showsPrec 11 n
        . showString " "
        . withSingI s showsPrec1 11 x
    Lookup i s -> showString "Lookup " . showsPrec 11 i . showString " " . showsPrec 11 s
