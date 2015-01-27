{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Cauterize.C11Files
  ( renderHFile
  , renderCFile
  , renderMetaHFile
  , renderMetaCFile
  , renderAssertions
  , renderMakefile
  ) where

import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Specification as Sp
import qualified Cauterize.Meta as M
import Cauterize.CSpec
import Cauterize.CMeta

import Data.Data

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T

import Paths_c11sync

data MetaInfo =
  MetaInfo { metaInfo :: CMeta
           , specInfo :: CSpec
           } deriving (Show, Data, Typeable)

renderHFile :: Sp.Spec -> IO Text
renderHFile s = renderFile (mkCSpec s) "templates/h_tmpl.h"

renderCFile :: Sp.Spec -> IO Text
renderCFile s = renderFile (mkCSpec s) "templates/c_tmpl.c"

renderMetaHFile :: Sp.Spec -> M.Meta -> IO Text
renderMetaHFile s a = renderMetaFile (mkCSpec s) (mkCMeta a) "templates/ai_h_tmpl.h"

renderMetaCFile :: Sp.Spec -> M.Meta -> IO Text
renderMetaCFile s a = renderMetaFile (mkCSpec s) (mkCMeta a) "templates/ai_c_tmpl.c"

renderAssertions :: Sp.Spec -> M.Meta -> IO Text
renderAssertions s a = renderMetaFile (mkCSpec s) (mkCMeta a) "templates/ai_assertions.tmpl.c"

renderMakefile :: Sp.Spec -> M.Meta -> IO Text
renderMakefile s a = renderMetaFile (mkCSpec s) (mkCMeta a) "templates/Makefile.tmpl"

renderMetaFile :: CSpec -> CMeta -> String -> IO Text
renderMetaFile s a p = do
    template <- getDataFileName p >>= T.readFile
    cfg <- mkCfg
    hastacheStr cfg (encodeStr $ T.unpack template) . mkGenericContext $ aiI
    where
      aiI = MetaInfo { metaInfo = a, specInfo = s }
      mkCfg = do
        tpath <- getDataFileName "templates/"
        return $ defaultConfig { muEscapeFunc = id
                               , muTemplateFileDir = Just tpath } :: IO (MuConfig IO)


renderFile :: CSpec -> String -> IO Text
renderFile s p = do
    template <- getDataFileName p >>= T.readFile
    cfg <- mkCfg
    hastacheStr cfg (encodeStr $ T.unpack template) . mkGenericContext $ s
    where
      mkCfg = do
        tpath <- getDataFileName "templates/"
        return $ defaultConfig { muEscapeFunc = id
                               , muTemplateFileDir = Just tpath } :: IO (MuConfig IO)
