{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.TH
    ( declareTables
    ) where

import Data.Kind (type (*))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH (Dec(DataD), Q, TyVarBndr(KindedTV), Type(ConT), mkName)

import Conkin.Extra (Always)
import qualified Conkin.Extra
import PersistWrap.BackEnd.Helper (AllEmbed)

declareTables :: String -> Q [Dec]
declareTables s = do
  argKind <- [t| Symbol -> * |]
  let n       = mkName s
      dataDec = DataD [] n [KindedTV (mkName "fk") argKind] Nothing [] []
  instanceDec <- [d| instance Always AllEmbed $(return $ ConT n) where withAlways = const id |]
  return $ dataDec : instanceDec
