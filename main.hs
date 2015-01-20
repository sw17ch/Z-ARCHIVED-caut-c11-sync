{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative

import qualified Cauterize.Specification as Sp
import qualified Cauterize.AI as AI

import Cauterize.FileNames
import Cauterize.C11Files

import System.Directory
import System.FilePath.Posix

import Data.Text.Lazy.IO as T

import Paths_c11sync

data Caut2C11Opts = Caut2C11Opts
  { inputFile :: String
  , aiInputFile :: Maybe String
  , outputDirectory :: String
  } deriving (Show)

optParser :: Parser Caut2C11Opts
optParser = Caut2C11Opts
  <$> strOption
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
  <*> (\m -> nullOption $ reader (\v -> return . Just $ v) `mappend` m)
    ( long "ai-input"
   <> metavar "AI_FILE_PATH"
   <> help "Input Agnostic Interface specification file."
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
caut2c11 opts = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out
  if fe || de
    then error $ out ++ " already exists."
    else go
  where
    out = outputDirectory opts

    loadSpec :: IO Sp.Spec
    loadSpec = do
      let inFile = inputFile opts
      s <- Sp.parseFile inFile
      case s of
        Left e -> error $ show e
        Right s' -> return s'

    loadAi :: IO (Maybe AI.Ai)
    loadAi = do
      let inFile = aiInputFile opts
      case inFile of
        Nothing -> return Nothing
        Just a -> do p <- AI.parseFile a
                     case p of
                       Left e -> error $ show e
                       Right a' -> return $ Just a'


    go = do
      createDirectory out
      s <- loadSpec
      a <- loadAi

      render s a out

render :: Sp.Spec -> Maybe AI.Ai -> String -> IO ()
render spec ai path = do
  copyFiles
  renderFiles
  maybe (return ()) renderAIFiles ai
  where
    renderAIFiles ai' = do
      renderAIHFile spec ai' >>= T.writeFile (path `combine` aihFileName spec)
      renderAICFile spec ai' >>= T.writeFile (path `combine` aicFileName spec)
      renderAssertions spec ai' >>= T.writeFile (path `combine` assertionsFileName spec)
      renderMakefile spec ai' >>= T.writeFile (path `combine` makefileName)

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
