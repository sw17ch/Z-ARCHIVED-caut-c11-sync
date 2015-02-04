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
  { cLibName :: Text -- ^ The name of the library suitable for C names.
  , cLibVersion :: Text -- ^ The version of the library as a C string.
  , cLibMinSize :: Word64 -- ^ The minimum encoded size.
  , cLibMaxSize :: Word64 -- ^ The maximum encoded size.
  , cLibHashStr :: Text -- ^ The library hash as comma-separated hex characters.
  , cLibTypes :: [CType] -- ^ The types from the schema with C details.
  } deriving (Data, Typeable, Show)

data CType = CType { ctName :: Text -- ^ The name of the type suitable for C names.
                   , ctMinSize :: Integer -- ^ The minimum packed size of the type.
                   , ctMaxSize :: Integer -- ^ The maximum size of the packed type.
                   , ctHashStr :: Text -- ^ The type hash as comma-separated hex characters.
                   , ctDetails :: CTypeDetails -- ^ Additional details for each type.
                   }
  deriving (Data, Typeable, Show)

data CTypeDetails = CBuiltIn     { ctdDecl :: Text, needTypeDef :: Bool }
                  | CArray       { ctdDecl :: Text, ctdReprName :: Text, ctdReprDecl :: Text, ctdArrayLen :: Integer }
                  | CVector      { ctdDecl :: Text, ctdReprName :: Text, ctdReprDecl :: Text, ctdVectorMaxLen :: Integer, ctdVectorMaxLenReprName :: Text, ctdVectorMaxLenReprDecl :: Text }
                  | CSynonym     { ctdDecl :: Text, ctdReprName :: Text, ctdReprDecl :: Text }
                  | CRecord      { ctdDecl :: Text, ctdFields :: [CNamedField] }
                  -- TODO: ctdRecordTagReprDecl should not be a repr decl. It
                  -- just happens to be. It should probably just be the
                  -- reprName.
                  | CUnion       { ctdDecl :: Text, ctdFields :: [CNamedField], ctdHasData :: Bool, ctdUnionTagReprName :: Text, ctdUnionTagReprDecl :: Text }
                  | CCombination { ctdDecl :: Text, ctdFields :: [CNamedField], ctdCombinationFlagsReprName :: Text, ctdCombinationFlagsReprDecl :: Text }
  deriving (Data, Typeable, Show)

{- | Named Fields are used to indicate the structure of fields in Enumerations,
 - Sets, and Structures. A Named Reference is used when a reference has
 - associated data. The Empty Reference is used when fields don't have
 - associated data. -}
data CNamedField = CNamedRef { cnrName :: Text
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
    Sp.Synonym { Sp.unSynonym = Sp.TSynonym { Sp.synonymRepr = r } } ->
      CSynonym { ctdDecl = d, ctdReprName = pack . show $ r, ctdReprDecl = builtInToStdType r }
    Sp.Array { Sp.unFixed = Sp.TArray { Sp.arrayRef = r, Sp.arrayLen = l } } ->
      CArray { ctdDecl = d, ctdReprName = pack r, ctdReprDecl = nameToDecl r , ctdArrayLen = l }
    Sp.Vector { Sp.unBounded = Sp.TVector { Sp.vectorRef = r, Sp.vectorMaxLen = l } , Sp.lenRepr = Sp.LengthRepr lr } ->
      CVector { ctdDecl = d
              , ctdReprName = pack r
              , ctdReprDecl = nameToDecl r
              , ctdVectorMaxLen = l
              , ctdVectorMaxLenReprName = pack . show $ lr
              , ctdVectorMaxLenReprDecl = builtInToStdType lr
              }
    Sp.Record { Sp.unRecord = Sp.TRecord { Sp.recordFields = Sp.Fields fs } } ->
      let fs' = P.map mkNamedRef' fs
      in CRecord { ctdDecl = d, ctdFields = fs' }
    Sp.Combination { Sp.unCombination = Sp.TCombination { Sp.combinationFields = Sp.Fields fs } , Sp.flagsRepr = Sp.FlagsRepr r } ->
      let fs' = P.map mkNamedRef' fs
      in CCombination { ctdDecl = d
                      , ctdFields = fs'
                      , ctdCombinationFlagsReprName = pack . show $ r
                      , ctdCombinationFlagsReprDecl = builtInToStdType r
                      }
    Sp.Union { Sp.unUnion = Sp.TUnion { Sp.unionFields = Sp.Fields fs } , Sp.tagRepr = Sp.TagRepr r } ->
      let fs' = P.map mkNamedRef' fs
      in CUnion { ctdDecl = d
                , ctdFields = fs'
                , ctdUnionTagReprName = pack . show $ r
                , ctdUnionTagReprDecl = builtInToStdType r
                , ctdHasData = hasData fs'
                }
  where
    mkNamedRef' = mkNamedRef nameToDecl'
    d = mkDecl t
    nameToDecl = nameToDecl' . pack

hasData :: [CNamedField] -> Bool
hasData [] = False
hasData (CNamedEmpty {}:fs) = hasData fs
hasData (CNamedRef {}:_) = True

mkNamedRef :: (Text -> Text) -> Sp.Field -> CNamedField
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
mkDecl t@(Sp.Record {}) = typeAsStruct t
mkDecl t@(Sp.Combination {}) = typeAsStruct t
mkDecl t@(Sp.Union {}) = typeAsStruct t
mkDecl t@(Sp.BuiltIn {}) = typeToName t
mkDecl t@(Sp.Synonym {}) = typeToName t

typeToName :: Sp.SpType -> Text
typeToName t = pack $ Sp.typeName t

typeAsStruct :: Sp.SpType -> Text
typeAsStruct t = "struct " `append` typeToName t
