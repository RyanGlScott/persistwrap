{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PersistWrap.Table.Schema.Internal where

import Control.Applicative (empty)
import Data.List (group, sort)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.Haskell.TH.Lift as TH
import Data.List.NonEmpty (NonEmpty)
import Data.Singletons.Prelude
import Data.Singletons.TH
import Test.QuickCheck (Arbitrary(..), Gen, elements, listOf1, oneof, shrinkList)

import Language.Haskell.TH.PromotedLift (PromotedLift(..))
import PersistWrap.Primitives (PrimName)
import PersistWrap.Table.Schema.Alphabet
import PersistWrap.Table.TH.Lift.Orphans ()

$(singletons [d|
  data BaseColumn text = Prim PrimName | Enum (NonEmpty text) | ForeignKey text | JSON
    deriving (Eq, Ord, Show)
  data Column text = Column Bool (BaseColumn text)
    deriving (Eq, Ord)
  data Schema text = Schema text [(text,Column text)]
    deriving (Eq, Ord)
  |])

$(singletonsOnly [d|
  schemaCols :: Schema Symbol -> [(Symbol, Column Symbol)]
  schemaCols (Schema _ cs) = cs
  schemaName :: Schema Symbol -> Symbol
  schemaName (Schema n _) = n
  |])

deriving instance TH.Lift (BaseColumn Text)
deriving instance TH.Lift (Column Text)

instance PromotedLift (BaseColumn Symbol) where
  promotedLift = \case
    Prim       pn   -> [t| 'Prim $(promotedLift pn) |]
    Enum       opts -> [t| 'Enum $(promotedLift opts) |]
    ForeignKey k    -> [t| 'ForeignKey $(promotedLift k) |]
    JSON            -> [t| 'JSON |]

instance PromotedLift (Column Symbol) where
  promotedLift (Column n bc) = [t| 'Column $(promotedLift n) $(promotedLift bc) |]

instance PromotedLift (Schema Symbol) where
  promotedLift (Schema n cols) = [t| 'Schema $(promotedLift n) $(promotedLift cols) |]

type TabSchema (tab :: (*, Schema Symbol)) = Snd tab
type TabName tab = SchemaName (TabSchema tab)
type TabCols tab = SchemaCols (TabSchema tab)

dedupSort :: Ord a => [a] -> [a]
dedupSort = map head . group . sort

arbitraryEnumOpts :: Gen (NonEmpty Text)
arbitraryEnumOpts = NonEmpty.fromList . dedupSort <$> listOf1 (elements natoAlphabet)

instance Arbitrary (BaseColumn Text) where
  arbitrary = oneof [Prim <$> arbitrary, Enum <$> arbitraryEnumOpts, pure JSON] -- TODO Foreign keys
  shrink    = \case
    Prim       p -> Prim <$> shrink p
    Enum       _ -> empty
    ForeignKey _ -> empty
    JSON         -> empty
instance Arbitrary (Column Text) where
  arbitrary = Column <$> arbitrary <*> arbitrary
  shrink (Column n bc) = Column n <$> shrink bc

genSchema :: Gen (Text -> Schema Text)
genSchema =
  (\cols name -> Schema name $ zip (map (Text.pack . ("col" ++) . show) [(0 :: Int) ..]) cols)
    <$> arbitrary

shrinkSchema :: Schema Text -> [Schema Text]
shrinkSchema (Schema name cols) = Schema name <$> shrinkList (const []) cols
