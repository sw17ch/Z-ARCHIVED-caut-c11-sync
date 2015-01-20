{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Cauterize.C11Files
  ( renderHFile
  , renderCFile
  , renderAIHFile
  , renderAICFile
  , renderAssertions
  , renderMakefile
  ) where

import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Specification as Sp
import qualified Cauterize.AI as AI
import Cauterize.CSpec
import Cauterize.CAi

import Data.Data

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T

import Paths_c11sync

data AiInfo =
  AiInfo { aiInfo :: CAi
         , specInfo :: CSpec
         } deriving (Show, Data, Typeable)

renderHFile :: Sp.Spec -> IO Text
renderHFile s = renderFile (mkCSpec s) "templates/h_tmpl.h"

renderCFile :: Sp.Spec -> IO Text
renderCFile s = renderFile (mkCSpec s) "templates/c_tmpl.c"

renderAIHFile :: Sp.Spec -> AI.Ai -> IO Text
renderAIHFile s a = renderAiFile (mkCSpec s) (mkCAi a) "templates/ai_h_tmpl.h"

renderAICFile :: Sp.Spec -> AI.Ai -> IO Text
renderAICFile s a = renderAiFile (mkCSpec s) (mkCAi a) "templates/ai_c_tmpl.c"

renderAssertions :: Sp.Spec -> AI.Ai -> IO Text
renderAssertions s a = renderAiFile (mkCSpec s) (mkCAi a) "templates/ai_assertions.tmpl.c"

renderMakefile :: Sp.Spec -> AI.Ai -> IO Text
renderMakefile s a = renderAiFile (mkCSpec s) (mkCAi a) "templates/Makefile.tmpl"

renderAiFile :: CSpec -> CAi -> String -> IO Text
renderAiFile s a p = do
    template <- getDataFileName p >>= T.readFile
    cfg <- mkCfg
    hastacheStr cfg (encodeStr $ T.unpack template) . mkGenericContext $ aiI
    where
      aiI = AiInfo { aiInfo = a, specInfo = s }
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
