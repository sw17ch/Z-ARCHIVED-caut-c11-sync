{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Options.Applicative

import qualified Cauterize.Specification as Sp

import Cauterize.FileNames
import Cauterize.C11Files

import System.FilePath.Posix
import System.Directory

import Data.Text.Lazy.IO as T

import Paths_caut_c11_sync

data Caut2C11Opts = Caut2C11Opts
  { specFile :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)

optParser :: Parser Caut2C11Opts
optParser = Caut2C11Opts
  <$> strOption
    ( long "spec"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
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
caut2c11 opts = createGuard out go
  where
    out = outputDirectory opts

    loadSpec :: IO Sp.Spec
    loadSpec = do
      let inFile = specFile opts
      s <- Sp.parseFile inFile
      case s of
        Left e -> error $ show e
        Right s' -> return s'

    go = loadSpec >>= flip render out

render :: Sp.Spec -> String -> IO ()
render spec path = do
  copyFiles
  renderFiles
  where
    renderFiles = do
      renderHFile spec >>= T.writeFile (path `combine` hFileName spec)
      renderCFile spec >>= T.writeFile (path `combine` cFileName spec)

    copyFiles = do
      cauterize_dot_h <- getDataFileName "support/lib/cauterize.h"
      cauterize_dot_c <- getDataFileName "support/lib/cauterize.c"
      greatest_dot_h <- getDataFileName "support/greatest.h"
      socket99_h <- getDataFileName "support/socket99.h"
      socket99_c <- getDataFileName "support/socket99.c"
      copyFile cauterize_dot_h (path `combine` "cauterize.h")
      copyFile cauterize_dot_c (path `combine` "cauterize.c")
      copyFile greatest_dot_h (path `combine` "greatest.h")
      copyFile socket99_h (path `combine` "socket99.h")
      copyFile socket99_c (path `combine` "socket99.c")
