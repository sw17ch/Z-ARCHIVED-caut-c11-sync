{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Format
  ( libName
  , libVersion
  , libHashToText
  ) where

import Cauterize.FormHash
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

