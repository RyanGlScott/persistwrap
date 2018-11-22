{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Table.BackEnd.TH
    ( declareTables
    ) where

import Data.Constraint (Dict(Dict))
import Data.Kind (type (*))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH (Dec(DataD), Q, TyVarBndr(KindedTV), Type(ConT), mkName)

import PersistWrap.Conkin.Extra (Always)
import qualified PersistWrap.Conkin.Extra
import PersistWrap.Table.BackEnd.Helper (AllEmbed)

declareTables :: String -> Q [Dec]
declareTables s = do
  argKind <- [t| Symbol -> * |]
  let n = mkName s
      dataDec = DataD [] n [KindedTV (mkName "fk") argKind] Nothing [] []
  instanceDec <-
    [d|
      instance Always AllEmbed $(return $ ConT n) where dict = Dict
      |]
  return $ dataDec : instanceDec
