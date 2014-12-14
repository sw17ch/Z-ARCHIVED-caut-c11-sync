{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
module Cauterize.CSpec where

import Prelude as P

import Data.Data
import Data.Word
import Data.Text.Lazy as T

import qualified Cauterize.Specification as Sp
import qualified Cauterize.Common.Types as Sp
import Cauterize.Format

data CSpec = CSpec
  { cLibName :: Text
  , cLibVersion :: Text
  , cLibMinSize :: Word64
  , cLibMaxSize :: Word64
  , cLibTypes :: [CType]
  } deriving (Data, Typeable, Show)

data CType = CType { ctName :: Text
                   , ctMinSize :: Integer
                   , ctMaxSize :: Integer
                   , ctHashStr :: Text
                   , ctDetails :: CTypeDetails
                   }
  deriving (Data, Typeable, Show)

data CTypeDetails = CBuiltIn
                  | CScalar
                  | CConst
                  | CArray
                  | CVector
                  | CStruct
                  | CSet
                  | CEnum
                  | CPad
  deriving (Data, Typeable, Show)

mkCSpec :: Sp.Spec -> CSpec
mkCSpec s = CSpec { cLibName = libName s
                  , cLibVersion = libVersion s
                  , cLibMinSize = fromIntegral . Sp.minSize . Sp.specSize $ s
                  , cLibMaxSize = fromIntegral . Sp.maxSize . Sp.specSize $ s
                  , cLibTypes = P.map mkCType $ Sp.specTypes s
                  }

mkCType :: Sp.SpType -> CType
mkCType t = CType { ctName = pack $ Sp.typeName t
                  , ctMinSize = Sp.minSize t
                  , ctMaxSize = Sp.maxSize t
                  , ctHashStr = libHashToText . Sp.spHash $ t
                  , ctDetails = mkCTypeDetails t
                  }

mkCTypeDetails :: Sp.SpType -> CTypeDetails
mkCTypeDetails Sp.BuiltIn { Sp.unBuiltIn = Sp.TBuiltIn {} } = CBuiltIn
mkCTypeDetails Sp.Scalar  { Sp.unScalar  = Sp.TScalar  {} } = CScalar
mkCTypeDetails Sp.Const   { Sp.unConst   = Sp.TConst   {} } = CConst
mkCTypeDetails Sp.Array   { Sp.unFixed   = Sp.TArray   {} } = CArray
mkCTypeDetails Sp.Vector  { Sp.unBounded = Sp.TVector  {} } = CVector
mkCTypeDetails Sp.Struct  { Sp.unStruct  = Sp.TStruct  {} } = CStruct
mkCTypeDetails Sp.Set     { Sp.unSet     = Sp.TSet     {} } = CSet
mkCTypeDetails Sp.Enum    { Sp.unEnum    = Sp.TEnum    {} } = CEnum
mkCTypeDetails Sp.Pad     { Sp.unPad     = Sp.TPad     {} } = CPad

{-
mkCType t@(Sp.BuiltIn {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Scalar {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Const {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Array {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Vector {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Struct {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Set {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Enum {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
mkCType t@(Sp.Pad {}) = CBuiltIn { ctName = pack $ Sp.typeName t }
-}
