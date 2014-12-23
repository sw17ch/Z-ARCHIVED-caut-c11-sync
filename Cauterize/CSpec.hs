{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Cauterize.CSpec where

import Prelude as P

import Data.Data
import Data.Word
import Data.List as L
import qualified Data.Map as M
import Data.Maybe
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

data CTypeDetails = CBuiltIn { ctdDecl :: Text, needTypeDef :: Bool }
                  | CConst   { ctdDecl :: Text, ctdReprName :: Text, ctdReprDecl :: Text, ctdConstVal :: Integer }
                  | CArray   { ctdDecl :: Text, ctdReprName :: Text, ctdReprDecl :: Text, ctdArrayLen :: Integer }
                  | CVector  { ctdDecl :: Text, ctdReprName :: Text, ctdReprDecl :: Text, ctdVectorMaxLen :: Integer, ctdVectorMaxLenReprDecl :: Text }
                  | CScalar  { ctdDecl :: Text, ctdReprName :: Text, ctdReprDecl :: Text }
                  | CStruct  { ctdDecl :: Text, ctdFields :: [CNamedRef] }
                  | CEnum    { ctdDecl :: Text, ctdFields :: [CNamedRef], ctdEnumTagReprDecl :: Text }
                  | CSet     { ctdDecl :: Text, ctdFields :: [CNamedRef], ctdSetFlagsReprDecl :: Text }
                  | CPad     { ctdDecl :: Text, ctdPadLen :: Integer }
  deriving (Data, Typeable, Show)

data CNamedRef = CNamedRef { cnrName :: Text
                           , cnrRefName :: Text
                           , cnrRefDecl :: Text
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
                  , cLibTypes = P.map (mkCType nameToDecl) $ Sp.specTypes s
                  }
  where
    nameToDeclMap = let s' = Sp.specTypes s
                        d = fmap mkDecl s'
                        n = fmap typeToName s'
                    in M.fromList $ L.zip n d

    nameToDecl :: Text -> Text
    nameToDecl n =  fromMaybe (error $ "Invalid name: " ++ unpack n ++ ".")
                              (M.lookup n nameToDeclMap)

mkCType :: (Text -> Text) -> Sp.SpType -> CType
mkCType nameToDecl t =
  CType { ctName = typeToName t
        , ctMinSize = Sp.minSize t
        , ctMaxSize = Sp.maxSize t
        , ctHashStr = libHashToText . Sp.spHash $ t
        , ctDetails = mkCTypeDetails nameToDecl t
        }

{- 
 - Convert the specification types into types that flatten out all of the C
 - type details. These should contain all the information necessary to emit C
 - code.
 -}
mkCTypeDetails :: (Text -> Text) -> Sp.SpType -> CTypeDetails
mkCTypeDetails nameToDecl' t =
  case t of
    Sp.BuiltIn { Sp.unBuiltIn = Sp.TBuiltIn { Sp.unTBuiltIn = b } } ->
      -- This is a glorious hack to work around `bool` being a meaningful phrase in C
      case b of
        Sp.BIbool -> CBuiltIn { ctdDecl = builtInToStdType b, needTypeDef = False }
        _ -> CBuiltIn { ctdDecl = builtInToStdType b, needTypeDef = True }
    Sp.Scalar { Sp.unScalar = Sp.TScalar { Sp.scalarRepr = r } } ->
      CScalar { ctdDecl = d, ctdReprName = builtInToStdType r, ctdReprDecl = builtInToStdType r }
    Sp.Const { Sp.unConst = Sp.TConst { Sp.constRepr = r, Sp.constValue = v } } ->
      CConst { ctdDecl = d, ctdReprName = builtInToStdType r, ctdReprDecl = builtInToStdType r , ctdConstVal  = v }
    Sp.Array { Sp.unFixed = Sp.TArray { Sp.arrayRef = r, Sp.arrayLen = l } } ->
      CArray { ctdDecl = d, ctdReprName = pack r, ctdReprDecl = nameToDecl r , ctdArrayLen = l }
    Sp.Vector { Sp.unBounded = Sp.TVector { Sp.vectorRef = r, Sp.vectorMaxLen = l } , Sp.lenRepr = Sp.LengthRepr lr } ->
      CVector { ctdDecl = d
              , ctdReprName = pack r
              , ctdReprDecl = nameToDecl r
              , ctdVectorMaxLen = l
              , ctdVectorMaxLenReprDecl = builtInToStdType lr
              }
    Sp.Struct { Sp.unStruct = Sp.TStruct { Sp.structFields = Sp.Fields fs } } ->
      CStruct { ctdDecl = d, ctdFields = P.map (mkNamedRef nameToDecl') fs }
    Sp.Set { Sp.unSet = Sp.TSet { Sp.setFields = Sp.Fields fs } , Sp.flagsRepr = Sp.FlagsRepr r } ->
      CSet { ctdDecl = d
           , ctdFields = P.map (mkNamedRef nameToDecl') fs
           , ctdSetFlagsReprDecl = builtInToStdType r
           }
    Sp.Enum { Sp.unEnum = Sp.TEnum { Sp.enumFields = Sp.Fields fs } , Sp.tagRepr = Sp.TagRepr r } ->
      CEnum { ctdDecl = d
            , ctdFields = P.map (mkNamedRef nameToDecl') fs
            , ctdEnumTagReprDecl = builtInToStdType r
            }
    Sp.Pad { Sp.unPad = Sp.TPad { Sp.padLength = l } } ->
      CPad { ctdDecl = d, ctdPadLen = l }
  where
    d = mkDecl t
    nameToDecl = nameToDecl' . pack

mkNamedRef :: (Text -> Text) -> Sp.Field -> CNamedRef
mkNamedRef nameToDecl Sp.Field { Sp.fName = n, Sp.fRef = r, Sp.fIndex = i } =
  CNamedRef { cnrName = pack n
            , cnrRefName = pack r
            , cnrRefDecl = nameToDecl . pack $ r
            , cnrIndex = i }
mkNamedRef _ Sp.EmptyField { Sp.fName = n, Sp.fIndex = i } =
  CNamedEmpty { cneName = pack n, cneIndex = i }

mkDecl :: Sp.SpType -> Text
mkDecl t@(Sp.Array {}) = typeAsStruct t
mkDecl t@(Sp.Vector {}) = typeAsStruct t
mkDecl t@(Sp.Struct {}) = typeAsStruct t
mkDecl t@(Sp.Set {}) = typeAsStruct t
mkDecl t@(Sp.Enum {}) = typeAsStruct t
mkDecl t@(Sp.Pad {}) = typeAsStruct t
mkDecl t@(Sp.BuiltIn {}) = typeToName t
mkDecl t@(Sp.Scalar {}) = typeToName t
mkDecl t@(Sp.Const {}) = typeToName t

typeToName :: Sp.SpType -> Text
typeToName t = pack $ Sp.typeName t

typeAsStruct :: Sp.SpType -> Text
typeAsStruct t = "struct " `append` typeToName t
