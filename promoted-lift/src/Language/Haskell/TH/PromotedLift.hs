{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.PromotedLift
    ( PromotedLift(..)
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Singletons (Demote, SingKind)
import Data.Singletons.TypeLits (Symbol)
import qualified Data.Text as Text

import Language.Haskell.TH (Q, TyLit(..), Type(..))

-- | fromSing (sing @_ @ $(promotedLift x) ) == x
class SingKind a => PromotedLift a where
  promotedLift :: Demote a -> Q Type

instance PromotedLift Bool where
  promotedLift = \case
    False -> [t| 'False |]
    True  -> [t| 'True |]
instance PromotedLift Symbol where
  promotedLift = return . LitT . StrTyLit . Text.unpack
instance PromotedLift a => PromotedLift [a] where
  promotedLift xs = foldr (AppT . AppT PromotedConsT) PromotedNilT <$> traverse promotedLift xs
instance PromotedLift a => PromotedLift (NonEmpty a) where
  promotedLift (x :| xs) = [t| $(promotedLift x) ':| $(promotedLift xs) |]
instance (PromotedLift a, PromotedLift b) => PromotedLift (a, b) where
  promotedLift (x, y) = [t| '( $(promotedLift x) , $(promotedLift y) ) |]
