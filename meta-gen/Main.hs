{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative

import qualified Cauterize.Specification as Sp
import qualified Cauterize.Meta as M

import Cauterize.FileNames
import Cauterize.C11Files

import System.FilePath.Posix

import Data.Text.Lazy.IO as T

data Caut2C11Opts = Caut2C11Opts
  { specFile :: String
  , metaFile :: String
  , outputDirectory :: String
  } deriving (Show)

optParser :: Parser Caut2C11Opts
optParser = Caut2C11Opts
  <$> strOption
    ( long "spec"
   <> metavar "SPEC_PATH"
   <> help "Cauterize specification file."
    )
  <*> strOption
    ( long "meta"
   <> metavar "META_PATH"
   <> help "Meta interface specification file."
    )
  <*> strOption
    ( long "output"
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )

options :: ParserInfo Caut2C11Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

runWithOptions :: (Caut2C11Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

main :: IO ()
main = runWithOptions caut2c11

caut2c11 :: Caut2C11Opts -> IO ()
caut2c11 opts = createGuard out $ do
  s <- loadSpec
  a <- loadMeta

  render s a out
  where
    out = outputDirectory opts

    loadSpec = do
      let inFile = specFile opts
      s <- Sp.parseFile inFile
      case s of
        Left e -> error $ show e
        Right s' -> return s'

    loadMeta = do
      let inFile = metaFile opts
      do p <- M.parseFile inFile
         case p of
           Left e -> error $ show e
           Right a' -> return a'

render :: Sp.Spec -> M.Meta -> String -> IO ()
render spec meta path = do
  renderMetaHFile spec meta >>= T.writeFile (path `combine` metaHFileName spec)
  renderMetaCFile spec meta >>= T.writeFile (path `combine` metaCFileName spec)
  renderAssertions spec meta >>= T.writeFile (path `combine` assertionsFileName spec)
  renderMakefile spec meta >>= T.writeFile (path `combine` makefileName)
