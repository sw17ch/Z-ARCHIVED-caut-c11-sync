{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Cauterize.CMeta where

import Prelude as P
import Data.Text.Lazy as T
import Data.Data

import Cauterize.Format
import qualified Cauterize.Meta as M

mkCMeta :: M.Meta -> CMeta
mkCMeta M.Meta { M.metaTypeLength = tl, M.metaDataLength = dl, M.metaTypes = ts } =
  CMeta { metaTypeLength = tl
        , metaDataLength = dl
        , metaDataLengthBuiltInRepr = builtInAsLengthInBytes dl
        , metaDataLengthDecl = unsignedAsLengthInBytes dl
        , metaTypes = fmap mkCMetaType ts
        }

data CMeta = CMeta
  { metaTypeLength :: Integer
  , metaTypes :: [CMetaType]
  , metaDataLength :: Integer
  , metaDataLengthBuiltInRepr :: Text
  , metaDataLengthDecl :: Text
  } deriving (Data, Typeable, Show)

data CMetaType = CMetaType
  { caiTypeName :: Text
  , caiPrefix :: Text
  } deriving (Data, Typeable, Show)

mkCMetaType :: M.MetaType -> CMetaType
mkCMetaType M.MetaType { M.metaTypeName = n, M.metaTypePrefix = p } =
  CMetaType { caiTypeName = pack n, caiPrefix = bytesToCSV p }
