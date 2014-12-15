{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Format
  ( libName
  , libVersion
  , libHashToText

  , cautNameToCName
  , builtInToStdType
  ) where

import Cauterize.FormHash
import qualified Cauterize.Common.Types as Sp
import qualified Cauterize.Specification as Sp
import Data.Text.Lazy as T
import Data.Word
import Numeric

libName :: Sp.Spec -> Text
libName s = "lib" `append` replace " " "_" (pack $ Sp.specName s)

libVersion :: Sp.Spec -> Text
libVersion = pack . Sp.specVersion

libHashToText :: FormHash -> Text
libHashToText h = let bs = hashToBytes h
                  in T.intercalate "," $ fmap showByte bs
  where
    showByte :: Word8 -> Text
    showByte b = let s = pack $ showHex b ""
                 in case T.length s of
                      2 -> "0x" `append` T.toUpper s
                      1 -> "0x0" `append` T.toUpper s
                      _ -> error "This should be impossible."

cautNameToCName :: Text -> Text
cautNameToCName n = n

builtInToStdType :: Sp.BuiltIn -> Text
builtInToStdType Sp.BIu8       = "uint8_t"
builtInToStdType Sp.BIu16      = "uint16_t"
builtInToStdType Sp.BIu32      = "uint32_t"
builtInToStdType Sp.BIu64      = "uint64_t"
builtInToStdType Sp.BIs8       = "int8_t"
builtInToStdType Sp.BIs16      = "int16_t"
builtInToStdType Sp.BIs32      = "int32_t"
builtInToStdType Sp.BIs64      = "int64_t"
builtInToStdType Sp.BIieee754s = "float"
builtInToStdType Sp.BIieee754d = "double"
builtInToStdType Sp.BIbool     = "caut_bool"
