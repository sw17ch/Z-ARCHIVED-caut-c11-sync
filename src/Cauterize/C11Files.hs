{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Cauterize.C11Files
  ( renderHFile
  , renderCFile
  , renderMetaHFile
  , renderMetaCFile
  , renderAssertions
  , renderTestClient
  , renderMakefile

  , createGuard
  ) where

import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Specification as Sp
import Cauterize.CSpec
import Cauterize.CMeta

import System.Directory

import Data.Data

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T

import Paths_caut_c11_sync

data MetaInfo =
  MetaInfo { metaInfo :: CMeta
           , specInfo :: CSpec
           } deriving (Show, Data, Typeable)

renderHFile :: Sp.Spec -> IO Text
renderHFile s = renderFile (mkCSpec s) "templates/h_tmpl.h"

renderCFile :: Sp.Spec -> IO Text
renderCFile s = renderFile (mkCSpec s) "templates/c_tmpl.c"

renderMetaHFile :: Sp.Spec -> IO Text
renderMetaHFile s = renderMetaFile (mkCSpec s) (mkCMeta s) "templates/meta_h_tmpl.h"

renderMetaCFile :: Sp.Spec -> IO Text
renderMetaCFile s = renderMetaFile (mkCSpec s) (mkCMeta s) "templates/meta_c_tmpl.c"

renderAssertions :: Sp.Spec -> IO Text
renderAssertions s = renderMetaFile (mkCSpec s) (mkCMeta s) "templates/meta_assertions.tmpl.c"

renderTestClient :: Sp.Spec -> IO Text
renderTestClient s = renderMetaFile (mkCSpec s) (mkCMeta s) "templates/test_client.tmpl.c"

renderMakefile :: Sp.Spec -> IO Text
renderMakefile s = renderMetaFile (mkCSpec s) (mkCMeta s) "templates/Makefile.tmpl"

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

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else if de
          then go
          else createDirectory out >> go
