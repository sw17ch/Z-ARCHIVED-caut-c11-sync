{-# LANGUAGE OverloadedStrings #-}
module Cauterize.FileNames
  ( hFileName
  ) where

import qualified Cauterize.Specification as Sp
import Data.Text.Lazy as T
import Cauterize.Format

hFileName :: Sp.Spec -> FilePath
hFileName s = unpack (libName s) ++ ".h"

