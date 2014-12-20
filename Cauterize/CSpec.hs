{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
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
  , cLibHashStr :: Text
  , cLibTypes :: [CType]
  } deriving (Data, Typeable, Show)

data CType = CType { ctName :: Text
                   , ctMinSize :: Integer
                   , ctMaxSize :: Integer
                   , ctHashStr :: Text
                   , ctDetails :: CTypeDetails
                   }
  deriving (Data, Typeable, Show)

data CTypeDetails = CBuiltIn { ctdStdType :: Text }
                  | CConst   { ctdReprName :: Text, ctdConstVal :: Integer }
                  | CArray   { ctdReprName :: Text, ctdArrayLen :: Integer }
                  | CVector  { ctdReprName :: Text, ctdVectorMaxLen :: Integer, ctdVectorMaxLenReprName :: Text }
                  | CScalar  { ctdReprName :: Text }
                  | CStruct  { ctdFields :: [CNamedRef] }
                  | CEnum    { ctdFields :: [CNamedRef], ctdEnumTagReprName :: Text }
                  | CSet     { ctdFields :: [CNamedRef], ctdSetFlagsReprName :: Text }
                  | CPad     { ctdPadLen :: Integer }
  deriving (Data, Typeable, Show)

data CNamedRef = CNamedRef { cnrName :: Text
                           , cnrRefName :: Text
                           , cnrIndex :: Integer
                           }
               | CNamedEmpty { cneName :: Text
                             , cneIndex :: Integer
                             }
  deriving (Data, Typeable, Show)

mkCSpec :: Sp.Spec -> CSpec
mkCSpec s = CSpec { cLibName = libName s
                  , cLibVersion = libVersion s
                  , cLibMinSize = fromIntegral . Sp.minSize . Sp.specSize $ s
                  , cLibMaxSize = fromIntegral . Sp.maxSize . Sp.specSize $ s
                  , cLibHashStr = libHashToText . Sp.specHash $ s
                  , cLibTypes = P.map mkCType $ Sp.specTypes s
                  }

mkCType :: Sp.SpType -> CType
mkCType t = CType { ctName = pack $ Sp.typeName t
                  , ctMinSize = Sp.minSize t
                  , ctMaxSize = Sp.maxSize t
                  , ctHashStr = libHashToText . Sp.spHash $ t
                  , ctDetails = mkCTypeDetails t
                  }

{- 
 - Convert the specification types into types that flatten out all of the C
 - type details. These should contain all the information necessary to emit C
 - code.
 -}
mkCTypeDetails :: Sp.SpType -> CTypeDetails
mkCTypeDetails Sp.BuiltIn { Sp.unBuiltIn = Sp.TBuiltIn { Sp.unTBuiltIn = b } } =
  CBuiltIn { ctdStdType = builtInToStdType b }
mkCTypeDetails Sp.Scalar { Sp.unScalar = Sp.TScalar { Sp.scalarRepr = r } } =
  CScalar { ctdReprName = cautNameToCName . pack $ show r }
mkCTypeDetails Sp.Const { Sp.unConst = Sp.TConst { Sp.constRepr = r, Sp.constValue = v } } =
  CConst { ctdReprName = cautNameToCName . pack $ show r
         , ctdConstVal  = v
         }
mkCTypeDetails Sp.Array { Sp.unFixed = Sp.TArray { Sp.arrayRef = r, Sp.arrayLen = l } } =
  CArray { ctdReprName = cautNameToCName . pack $ r
         , ctdArrayLen = l
         }
mkCTypeDetails Sp.Vector { Sp.unBounded = Sp.TVector { Sp.vectorRef = r, Sp.vectorMaxLen = l }
                         , Sp.lenRepr = (Sp.LengthRepr lr)
                         } =
  CVector { ctdReprName = cautNameToCName . pack $ r
          , ctdVectorMaxLen = l
          , ctdVectorMaxLenReprName = cautNameToCName . pack . show $ lr
          }
mkCTypeDetails Sp.Struct { Sp.unStruct = Sp.TStruct { Sp.structFields = Sp.Fields fs } } =
  CStruct { ctdFields = P.map mkNamedRef fs }
mkCTypeDetails Sp.Set { Sp.unSet = Sp.TSet { Sp.setFields = Sp.Fields fs }
                      , Sp.flagsRepr = Sp.FlagsRepr r
                      } =
  CSet { ctdFields = P.map mkNamedRef fs
       , ctdSetFlagsReprName = cautNameToCName . pack . show $ r
       }
mkCTypeDetails Sp.Enum { Sp.unEnum = Sp.TEnum { Sp.enumFields = Sp.Fields fs }
                       , Sp.tagRepr = Sp.TagRepr r
                       } =
  CEnum { ctdFields = P.map mkNamedRef fs
        , ctdEnumTagReprName = cautNameToCName . pack . show $ r
        }
mkCTypeDetails Sp.Pad { Sp.unPad = Sp.TPad { Sp.padLength = l } } =
  CPad { ctdPadLen = l }

mkNamedRef :: Sp.Field -> CNamedRef
mkNamedRef Sp.Field { Sp.fName = n, Sp.fRef = r, Sp.fIndex = i } = CNamedRef { cnrName = pack n, cnrRefName = pack r, cnrIndex = i }
mkNamedRef Sp.EmptyField { Sp.fName = n, Sp.fIndex = i } = CNamedEmpty { cneName = pack n, cneIndex = i }
