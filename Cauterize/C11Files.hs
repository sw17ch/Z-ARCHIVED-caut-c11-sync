{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Cauterize.C11Files
  ( renderHFile
  , renderCFile
  , renderAIHFile
  , renderAICFile
  , renderAssertions

  , mkTemplateInfo
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

data TemplateInfo =
  TemplateInfo { aiInfo :: CAi
               , specInfo :: CSpec
               } deriving (Show, Data, Typeable)

renderHFile :: TemplateInfo -> IO Text
renderHFile ti = renderFile ti "templates/h_tmpl.h"

renderCFile :: TemplateInfo -> IO Text
renderCFile ti = renderFile ti "templates/c_tmpl.c"

renderAIHFile :: TemplateInfo -> IO Text
renderAIHFile ti = renderFile ti "templates/ai_h_tmpl.h"

renderAICFile :: TemplateInfo -> IO Text
renderAICFile ti = renderFile ti "templates/ai_c_tmpl.c"

renderAssertions :: TemplateInfo -> IO Text
renderAssertions ti = renderFile ti "templates/assertions.tmpl.c"

mkTemplateInfo :: Sp.Spec -> AI.Ai -> TemplateInfo
mkTemplateInfo sp ai = TemplateInfo { aiInfo = cAi, specInfo = cSpec }
  where
    cAi = mkCAi ai
    cSpec = mkCSpec sp

renderFile :: TemplateInfo -> String -> IO Text
renderFile ti p = do
    template <- getDataFileName p >>= T.readFile
    cfg <- mkCfg
    hastacheStr cfg (encodeStr $ T.unpack template) . mkGenericContext $ ti
    where
      mkCfg = do
        tpath <- getDataFileName "templates/"
        return $ defaultConfig { muEscapeFunc = id
                               , muTemplateFileDir = Just tpath } :: IO (MuConfig IO)
