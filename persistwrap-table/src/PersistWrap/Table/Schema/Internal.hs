{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PersistWrap.Table.Schema.Internal where

import Data.Kind (type (*))
import Data.Text (Text)
import qualified Language.Haskell.TH.Lift as TH
import Data.List.NonEmpty (NonEmpty)
import Data.Singletons.Prelude hiding (type (*))
import Data.Singletons.TH

import Language.Haskell.TH.PromotedLift (PromotedLift(..))
import PersistWrap.Primitives (PrimName)
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
