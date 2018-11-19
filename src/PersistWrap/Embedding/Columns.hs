{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Embedding.Columns where

import Data.List.NonEmpty
import Data.Singletons.Prelude
import Data.Singletons.Prelude.IsString
import Data.Singletons.TH
import Data.String (IsString)

import PersistWrap.Structure.Primitives
import PersistWrap.Table as Table

$(singletons [d|
    containerColumnName, indexColumnName, keyColumnName, loneColumnName, tagColumnName
      :: IsString text => text
    containerColumnName = "_container"
    indexColumnName = "_index"
    keyColumnName = "_key"
    loneColumnName = "value"
    tagColumnName = "_tag"

    containerColumn :: text -> Column text
    containerColumn cname = Column False (Table.ForeignKey cname)
    indexColumn :: Column text
    indexColumn = Column False (Table.Prim PrimInt64)
    tagColumn :: NonEmpty text -> Column text
    tagColumn opts = Column False (Table.Enum opts)

    containerNamedColumn :: IsString text => text -> (text, Column text)
    containerNamedColumn cname = (containerColumnName, containerColumn cname)
    indexNamedColumn :: IsString text => (text, Column text)
    indexNamedColumn = (indexColumnName, indexColumn)
    tagNamedColumn :: IsString text => NonEmpty text -> (text, Column text)
    tagNamedColumn opts = (tagColumnName, tagColumn opts)
  |])
