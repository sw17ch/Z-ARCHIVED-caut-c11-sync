{-# LANGUAGE OverloadedStrings #-}
module Cauterize.C11Files
  ( renderHFile
  ) where

import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Specification as Sp
import Cauterize.CSpec

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T

import Paths_c11sync

renderHFile :: Sp.Spec -> IO Text
renderHFile s = do
    template <- getDataFileName "templates/hFile.tmpl.h" >>= T.readFile 
    print cSpec
    hastacheStr defaultConfig (encodeStr $ T.unpack template) . mkGenericContext $ cSpec
    where
      cSpec = mkCSpec s


-- import Text.PrettyPrint.Leijen.Text

