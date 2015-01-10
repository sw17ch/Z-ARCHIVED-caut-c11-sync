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
renderHFile s = renderFile s "templates/h_tmpl.h"

renderCFile :: Sp.Spec -> IO Text
renderCFile s = renderFile s "templates/c_tmpl.c"

renderFile :: Sp.Spec -> String -> IO Text
renderFile s p = do
    template <- getDataFileName p >>= T.readFile 
    cfg <- mkCfg
    hastacheStr cfg (encodeStr $ T.unpack template) . mkGenericContext $ cSpec
    where
      mkCfg = do
        tpath <- getDataFileName "templates/"
        return $ defaultConfig { muEscapeFunc = id, muTemplateFileDir = Just tpath } :: IO (MuConfig IO)
      cSpec = mkCSpec s


-- import Text.PrettyPrint.Leijen.Text

