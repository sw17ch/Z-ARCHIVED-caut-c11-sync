{-# LANGUAGE OverloadedStrings #-}
module Cauterize.C11Files
  ( renderHFile
  , renderCFile
  ) where

import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Specification as Sp
import Cauterize.CSpec

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T

import Paths_c11sync

renderHFile :: Sp.Spec -> IO Text
renderHFile s = renderFile s "templates/hFile.tmpl.h"

renderCFile :: Sp.Spec -> IO Text
renderCFile s = renderFile s "templates/cFile.tmpl.c"

renderFile :: Sp.Spec -> String -> IO Text
renderFile s p = do
    template <- getDataFileName p >>= T.readFile 
    hastacheStr defaultConfig (encodeStr $ T.unpack template) . mkGenericContext $ cSpec
    where
      cSpec = mkCSpec s


-- import Text.PrettyPrint.Leijen.Text

