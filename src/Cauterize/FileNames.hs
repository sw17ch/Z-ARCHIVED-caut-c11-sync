{-# LANGUAGE OverloadedStrings #-}
module Cauterize.FileNames
  ( hFileName
  , cFileName
  , metaHFileName
  , metaCFileName
  , assertionsFileName
  , makefileName
  ) where

import qualified Cauterize.Specification as Sp
import Data.Text.Lazy as T
import Cauterize.Format

hFileName :: Sp.Spec -> FilePath
hFileName s = unpack (libName s) ++ ".h"

cFileName :: Sp.Spec -> FilePath
cFileName s = unpack (libName s) ++ ".c"

metaHFileName :: Sp.Spec -> FilePath
metaHFileName s = unpack (libName s) ++ "_meta.h"

metaCFileName :: Sp.Spec -> FilePath
metaCFileName s = unpack (libName s) ++ "_meta.c"

assertionsFileName :: Sp.Spec -> FilePath
assertionsFileName s = unpack (libName s) ++ "_assertions.c"

makefileName :: FilePath
makefileName = "Makefile"
