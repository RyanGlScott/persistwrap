module PersistWrap.Persistable.Schemas
   ( repToSchemas
   ) where

import Conkin (Tuple(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, maybeToList)
import Data.Singletons (SomeSing(SomeSing), fromSing)
import Data.Singletons.TypeLits (Symbol)
import Data.Text (Text)

import Conkin.Extra (mapUncheck)
import PersistWrap.Persistable.Columns
import PersistWrap.Persistable.Rep
import PersistWrap.Persistable.Utils
import PersistWrap.Table as Table

listKeys :: Text -> [(Text, Column Text)]
listKeys containerName = [containerNamedColumn containerName, indexNamedColumn]

mapKeys :: Text -> Maybe (Column Text) -> [(Text, Column Text)]
mapKeys containerName keyCol =
  containerNamedColumn containerName : [ (keyColumnName, kc) | kc <- maybeToList keyCol ]

colRepToSchemas :: Text -> ColumnRep fk structure -> (Maybe (Column Text), [Schema Text])
colRepToSchemas selfSchemaName = \case
  UnitRep _  -> (Nothing, [])
  PrimRep c  -> (Just $ fromSing c, [])
  FnRep cr _ -> colRepToSchemas selfSchemaName cr
  ForeignRep subRep@(NamedSchemaRep (fromSing -> tabName) _) ->
    let (sch1, otherSchemas) = repToSchemas subRep
    in  (Just $ Column False (ForeignKey tabName), sch1 : otherSchemas)
  NullForeignRep subRep@(NamedSchemaRep (fromSing -> tabName) _) ->
    let (sch1, otherSchemas) = repToSchemas subRep
    in  (Just $ Column True (ForeignKey tabName), sch1 : otherSchemas)
  ListRep subRep ->
    let (Schema subName subcols, otherSchemas) = repToSchemas subRep
    in  (Nothing, Schema subName (listKeys selfSchemaName ++ subcols) : otherSchemas)
  MapRep subKey subVal ->
    let (Schema subName subcols, otherSchemas2) = repToSchemas subVal
        (kc                    , otherSchemas1) = colRepToSchemas subName subKey
    in  ( Nothing
        , Schema subName (mapKeys selfSchemaName kc ++ subcols) : otherSchemas1 ++ otherSchemas2
        )

consTagColumn :: SomeSing (NonEmpty Symbol) -> Schema Text -> Schema Text
consTagColumn (SomeSing (fromSing -> tags)) (Schema schemaName cols) =
  Schema schemaName (tagNamedColumn tags : cols)

repToSchemas :: NamedSchemaRep fk schemaName nx -> (Schema Text, [Schema Text])
repToSchemas (NamedSchemaRep (fromSing -> selfSchemaName) rep) = case rep of
  AtMostOneColumnSchema cr ->
    let (c, others) = colRepToSchemas selfSchemaName cr
    in  (Schema selfSchemaName (maybeToList $ (loneColumnName, ) <$> c), others)
  ProductSchema cols        -> collectColumns selfSchemaName cols
  SumUnIndexedSchema _ cols -> collectColumns selfSchemaName cols
  SumIndexedSchema cols ->
    let (newSchema, others) = collectColumns selfSchemaName cols
    in  (consTagColumn (getNonEmptyTags cols) newSchema, others)

collectColumns :: Text -> Tuple xs (NamedColumnRep fk) -> (Schema Text, [Schema Text])
collectColumns selfSchemaName cols =
  let (mcols, schs) = unzip $ mapUncheck
        (\(NamedColumnRep sym col) ->
          let (c, others) = colRepToSchemas selfSchemaName col in ((fromSing sym, ) <$> c, others)
        )
        cols
  in  (Schema selfSchemaName (catMaybes mcols), concat schs)
