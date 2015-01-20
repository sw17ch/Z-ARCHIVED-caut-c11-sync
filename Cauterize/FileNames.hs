{-# LANGUAGE OverloadedStrings #-}
module Cauterize.FileNames
  ( hFileName
  , cFileName
  , aihFileName
  , aicFileName
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

aihFileName :: Sp.Spec -> FilePath
aihFileName s = unpack (libName s) ++ "_ai.h"

aicFileName :: Sp.Spec -> FilePath
aicFileName s = unpack (libName s) ++ "_ai.c"

assertionsFileName :: Sp.Spec -> FilePath
assertionsFileName s = unpack (libName s) ++ "_assertions.c"

makefileName :: FilePath
makefileName = "Makefile"
