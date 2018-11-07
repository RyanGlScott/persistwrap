module PersistWrap.Embedding.Schemas
   ( ColumnRep(..)
   , SchemaRep(..)
   , getSchemaRep
   , repToSchemas
   ) where

import Conkin (Tuple(..))
import Data.Bifunctor (first, second)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, maybeToList)
import Data.Singletons (fromSing)
import Data.Text (Text)

import PersistWrap.Conkin.Extra (mapUncheck, mapUncheckNonEmpty)
import PersistWrap.Embedding.Rep
import PersistWrap.Structure as Structure hiding (ForeignKey)
import PersistWrap.Table as Table

listKeys :: Text -> [(Text, Column Text)]
listKeys containerName =
  [ ("_container", Column False (ForeignKey containerName))
  , ("_index"    , Column False (Table.Prim PrimInt64))
  ]

mapKeys :: Text -> Maybe (Column Text) -> [(Text, Column Text)]
mapKeys containerName keyCol =
  ("_container", Column False (ForeignKey containerName))
    : [ ("_key", kc) | kc <- maybeToList keyCol ]

colRepToSchemas :: Text -> ColumnRep fk structure -> (Maybe (Column Text), [Schema Text])
colRepToSchemas selfSchemaName = \case
  UnitRep _  -> (Nothing, [])
  PrimRep c  -> (Just $ fromSing c, [])
  FnRep cr _ -> colRepToSchemas selfSchemaName cr
  ForeignRep subRep@(NamedSchemaRep tabName _) ->
    let (sch1, otherSchemas) = repToBuildSchemas subRep
    in  (Just $ Column False (ForeignKey tabName), sch1 : otherSchemas)
  NullForeignRep subRep@(NamedSchemaRep tabName _) ->
    let (sch1, otherSchemas) = repToBuildSchemas subRep
    in  (Just $ Column True (ForeignKey tabName), sch1 : otherSchemas)
  ListRep subRep ->
    let (Schema subName subcols, otherSchemas) = repToBuildSchemas subRep
    in  (Nothing, Schema subName (listKeys selfSchemaName ++ subcols) : otherSchemas)
  MapRep subKey subVal ->
    let (Schema subName subcols, otherSchemas2) = repToBuildSchemas subVal
        (kc                    , otherSchemas1) = colRepToSchemas subName subKey
    in  ( Nothing
        , Schema subName (mapKeys selfSchemaName kc ++ subcols) : otherSchemas1 ++ otherSchemas2
        )

makeNullable :: Column Text -> Column Text
makeNullable (Column _ bc) = Column True bc

mkAllNullable :: Schema Text -> Schema Text
mkAllNullable (Schema schemaName cols) = Schema schemaName (map (second makeNullable) cols)

consTagColumn :: NonEmpty Text -> Schema Text -> Schema Text
consTagColumn (tagHead :| tagTail) (Schema schemaName cols) =
  let newcol = ("_tag", Column False (Enum tagHead tagTail)) in Schema schemaName (newcol : cols)

repToBuildSchemas :: NamedSchemaRep fk nx -> (Schema Text, [Schema Text])
repToBuildSchemas (NamedSchemaRep selfSchemaName rep) = case rep of
  AtMostOneColumnSchema cr ->
    let (c, others) = colRepToSchemas selfSchemaName cr
    in  (Schema selfSchemaName (maybeToList $ ("value", ) <$> c), others)
  ProductSchema cols        -> collectColumns selfSchemaName cols
  SumUnIndexedSchema _ cols -> first mkAllNullable $ collectColumns selfSchemaName cols
  SumIndexedSchema cols ->
    let (newSchema, others) = collectColumns selfSchemaName cols
    in  ( consTagColumn (mapUncheckNonEmpty (\(NamedColumnRep n _) -> fromSing n) cols)
                        (mkAllNullable newSchema)
        , others
        )

repToSchemas :: NamedSchemaRep fk nx -> [Schema Text]
repToSchemas = uncurry (:) . repToBuildSchemas

collectColumns :: Text -> Tuple xs (NamedColumnRep fk) -> (Schema Text, [Schema Text])
collectColumns selfSchemaName cols =
  let (mcols, schs) = unzip $ mapUncheck
        (\(NamedColumnRep sym col) ->
          let (c, others) = colRepToSchemas selfSchemaName col in ((fromSing sym, ) <$> c, others)
        )
        cols
  in  (Schema selfSchemaName (catMaybes mcols), concat schs)
