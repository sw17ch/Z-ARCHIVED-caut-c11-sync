{-# LANGUAGE OverloadedStrings #-}
module Cauterize.FileNames
  ( hFileName
  , cFileName
  ) where

import qualified Cauterize.Specification as Sp
import Data.Text.Lazy as T
import Cauterize.Format

hFileName :: Sp.Spec -> FilePath
hFileName s = unpack (libName s) ++ ".h"

cFileName :: Sp.Spec -> FilePath
cFileName s = unpack (libName s) ++ ".c"

