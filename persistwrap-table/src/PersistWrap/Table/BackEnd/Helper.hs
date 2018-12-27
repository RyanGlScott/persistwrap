module PersistWrap.Table.BackEnd.Helper
    ( TableMap
    , checkForDuplicates
    , constructMap
    ) where

import Conkin (Tuple)
import Data.List (group, sort)
import Data.Singletons
import Data.Singletons.Prelude (SList)
import Data.Singletons.TypeLits (Symbol)
import GHC.Stack (HasCallStack)

import Consin (Some, mapUncheckSing, some)
import Consin.SingMap (SingMap)
import qualified Consin.SingMap as SingMap
import PersistWrap.Table

type TableMap tab = SingMap (SomeTableNamed tab)

checkForDuplicates :: HasCallStack => SList (schemas :: [Schema Symbol]) -> y -> y
checkForDuplicates sschemas y
  | anyDuplicates (map (\(Schema name _) -> name) (fromSing sschemas))
  = error $ "Schema names are not distinct: " ++ show (fromSing sschemas)
  | otherwise
  = y

anyDuplicates :: Ord x => [x] -> Bool
anyDuplicates = any (\grp -> length grp > 1) . group . sort

constructMap :: SList schemas -> Tuple schemas tab -> TableMap tab
constructMap schemas = SingMap.fromList . mapUncheckSing schemas tableToMapEntry

tableToMapEntry :: forall tab schema . SingI schema => tab schema -> Some (SomeTableNamed tab)
tableToMapEntry = case sing @schema of
  SSchema name cols -> some name . SomeTableNamed cols
