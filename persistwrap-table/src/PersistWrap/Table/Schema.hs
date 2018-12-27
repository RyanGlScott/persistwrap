{-# OPTIONS_GHC -Wno-orphans #-}

module PersistWrap.Table.Schema
  ( showPromotedSchema
  , module X
  ) where

import Data.Singletons (fromSing)
import Data.Singletons.ShowSing (ShowSing)
import Data.Singletons.TypeLits (Symbol)
import Data.Text (Text)

import PersistWrap.Table.Schema.Internal as X
import qualified PersistWrap.Table.Schema.Simple as Simple

schShow :: String -> Int -> Schema Text -> ShowS
schShow constrName d (Simple.fromSchema -> (schemaName, schemaCols)) =
  showParen (d > 10)
    $ showString constrName
    . showString " "
    . showsPrec 11 schemaName
    . showString " "
    . showsPrec 11 schemaCols

instance Show (Schema Text) where
  showsPrec = schShow "toSchema"

showPromotedSchema :: SSchema (schema :: Schema Symbol) -> ShowS
showPromotedSchema schema = showString "$" . schShow "schema" 11 (fromSing schema)

instance ShowSing (Schema Symbol) where
instance Show (SSchema (schema :: Schema Symbol)) where
  showsPrec d schema = showParen (d > 10) $ showString "sing @_ @" . showPromotedSchema schema
