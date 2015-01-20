{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Cauterize.CAi where

import Prelude as P
import Data.Text.Lazy as T
import Data.Data

import Cauterize.Format
import qualified Cauterize.AI as AI

mkCAi :: AI.Ai -> CAi
mkCAi AI.Ai { AI.aiTypeLength = tl, AI.aiDataLength = dl, AI.aiTypes = ts } =
  CAi { aiTypeLength = tl
      , aiDataLength = dl
      , aiDataLengthBuiltInRepr = builtInAsLengthInBytes dl
      , aiDataLengthDecl = unsignedAsLengthInBytes dl
      , aiTypes = fmap mkCAiType ts
      }

data CAi = CAi
  { aiTypeLength :: Integer
  , aiTypes :: [CAiType] -- See Cauterize.AI.Types for details on this structure
  , aiDataLength :: Integer
  , aiDataLengthBuiltInRepr :: Text
  , aiDataLengthDecl :: Text
  } deriving (Data, Typeable, Show)

data CAiType = CAiType
  { caiTypeName :: Text
  , caiPrefix :: Text
  } deriving (Data, Typeable, Show)

mkCAiType :: AI.AiType -> CAiType
mkCAiType AI.AiType { AI.aiTypeName = n, AI.aiTypePrefix = p } =
  CAiType { caiTypeName = pack n, caiPrefix = bytesToCSV p }
