{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.BackEnd.Helper
    ( AllEmbed
    , Emb
    , FK
    , ForeignKeysShowable
    , Items
    , ItemizedIn
    , setupHelper
    ) where

import Data.Constraint (Dict(Dict))
import Data.Singletons.Prelude hiding (All, Map)
import Data.Text (Text)

import PersistWrap.Conkin.Extra
import qualified PersistWrap.Conkin.Extra as All (All(..))
import qualified PersistWrap.Conkin.Extra as Always (Always(..))
import PersistWrap.Embedding.Class.Embeddable (HasRep, entitySchemas)
import PersistWrap.Embedding.Class.Embedded
import PersistWrap.Structure (StructureOf)
import PersistWrap.Table.Column
import PersistWrap.Table.Transactable

class HasRep (Fst schx) (StructureOf (Snd schx)) => EmbedPair schx
instance HasRep schemaName (StructureOf x) => EmbedPair '(schemaName,x)

type family Items x :: [(Symbol,*)]

class All EmbedPair (Items x) => AllEmbed x
instance All EmbedPair (Items x) => AllEmbed x

setupHelper
  :: forall fnitems m n x
   . (MonadDML m, Always AllEmbed fnitems)
  => (forall (sch :: [Schema Symbol]) . SList sch -> m x -> n x)
  -> Itemized (Items (fnitems (FK m))) m x
  -> n x
setupHelper setup action = case Always.dict @AllEmbed @fnitems @(FK m) of
  Dict ->
    let schemas = concat $ mapUncheck schemasOf (All.dicts @EmbedPair @(Items (fnitems (FK m))))
    in  withSomeSing schemas $ \sschemas -> setup sschemas (runItemized action)

schemasOf :: forall schx . DictC EmbedPair schx -> [Schema Text]
schemasOf (DictC Dict) = entitySchemas @(Fst schx) @(StructureOf (Snd schx))

type ForeignKeysShowable m = AlwaysS Show (FK m)

type ItemizedIn ctxt m = Itemized (Items (ctxt (FK m))) m

type Emb name x m = Embedded name x (Transaction m)
