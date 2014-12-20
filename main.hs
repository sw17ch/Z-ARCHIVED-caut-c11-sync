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
   <> value Nothing
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
    else go $ inputFile opts
  where
    out = outputDirectory opts
    go inFile = do
      s <- Sp.parseFile inFile
      case s of
        Left e -> print e
        Right s' -> do
          createDirectory out
          render s' out
          case aiInputFile opts of
            Just path -> do
              p <- AI.parseFile path
              case p of
                Left e -> print e
                Right ai -> renderAiFiles s' ai out
            Nothing -> return ()
  
render :: Sp.Spec -> String -> IO ()
render spec path = do
  renderHFile spec >>= T.writeFile (path `combine` hFileName spec) 
  renderCFile spec >>= T.writeFile (path `combine` cFileName spec)
  -- writeFile (path `combine` aFileName spec) aFile
  -- writeFile (path `combine` tFileName spec) tFile

  cauterize_dot_h <- getDataFileName "support/lib/cauterize.h"
  cauterize_dot_c <- getDataFileName "support/lib/cauterize.c"
  greatest_dot_h <- getDataFileName "support/greatest.h"
  socket99_h <- getDataFileName "support/socket99.h"
  socket99_c <- getDataFileName "support/socket99.c"
  makefile <- getDataFileName "support/Makefile"

  copyFile cauterize_dot_h (path `combine` "cauterize.h")
  copyFile cauterize_dot_c (path `combine` "cauterize.c")
  copyFile greatest_dot_h (path `combine` "greatest.h")
  copyFile makefile (path `combine` "Makefile")
  copyFile socket99_h (path `combine` "socket99.h")
  copyFile socket99_c (path `combine` "socket99.c")

renderAiFiles :: Sp.Spec -> AI.Ai -> FilePath -> IO ()
renderAiFiles _ _ _ = return ()
