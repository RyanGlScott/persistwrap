{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.BackEnd.Helper
    ( AllEmbed
    , ForeignKey
    , Items
    , setupHelper
    ) where

import Data.Constraint (Dict(Dict))
import Data.Singletons.Prelude hiding (All, Map)
import Data.Text (Text)

import Conkin.Extra
import qualified Conkin.Extra as All (All(..))
import PersistWrap (Items)
import PersistWrap.Itemized (Itemized(runItemized))
import PersistWrap.Persistable (HasRep, entitySchemas)
import PersistWrap.Structure (StructureOf)
import PersistWrap.Table (ForeignKey, Schema)

class HasRep (Fst schx) (StructureOf (Snd schx)) => EmbedPair schx
instance HasRep schemaName (StructureOf x) => EmbedPair '(schemaName,x)

class All EmbedPair (Items x) => AllEmbed x
instance All EmbedPair (Items x) => AllEmbed x

setupHelper
  :: forall fnitems m n x
   . Always AllEmbed fnitems
  => (forall (sch :: [Schema Symbol]) . SList sch -> m x -> n x)
  -> Itemized (Items (fnitems (ForeignKey m))) m x
  -> n x
setupHelper setup action =
  withAlways @AllEmbed @fnitems @(ForeignKey m)
    $ let schemas =
            concat $ mapUncheck schemasOf (All.dicts @EmbedPair @(Items (fnitems (ForeignKey m))))
      in  withSomeSing schemas $ \sschemas -> setup sschemas (runItemized action)

schemasOf :: forall schx . DictC EmbedPair schx -> [Schema Text]
schemasOf (DictC Dict) = entitySchemas @(Fst schx) @(StructureOf (Snd schx))
