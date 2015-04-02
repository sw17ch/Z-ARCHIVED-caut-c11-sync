{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Cauterize.CMeta where

import Prelude as P
import qualified Data.Text.Lazy as T
import Data.Data

import Cauterize.Format
import Cauterize.FormHash
import qualified Cauterize.Specification as S

mkCMeta :: S.Spec -> CMeta
mkCMeta S.Spec { S.specTypeTagWidth = tl
               , S.specLengthTagWidth = dl
               , S.specTypes = ts } =
  CMeta { metaTypeLength = tl'
        , metaDataLength = dl'
        , metaDataLengthBuiltInRepr = builtInAsLengthInBytes dl'
        , metaDataLengthDecl = unsignedAsLengthInBytes dl'
        , metaTypes = fmap (mkCMetaType $ fromIntegral tl') ts
        }
  where
    tl' = S.unTypeTagWidth tl
    dl' = S.unLengthTagWidth dl

data CMeta = CMeta
  { metaTypeLength :: Integer
  , metaTypes :: [CMetaType]
  , metaDataLength :: Integer
  , metaDataLengthBuiltInRepr :: T.Text
  , metaDataLengthDecl :: T.Text
  } deriving (Data, Typeable, Show)

data CMetaType = CMetaType
  { cMetaTypeName :: T.Text
  , cMetaPrefix :: T.Text
  } deriving (Data, Typeable, Show)

mkCMetaType :: Int -> S.SpType -> CMetaType
mkCMetaType plen t =
  CMetaType { cMetaTypeName = S.typeName t
            , cMetaPrefix = bytesToCSV prefix }
  where
    prefix = take plen $ hashToBytes $ S.spHash t
