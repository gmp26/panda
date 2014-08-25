{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Panda.Options where

import Control.Monad (when)
import System.Console.CmdArgs
import System.FilePath ()
import Data.Version (showVersion)
import Paths_panda (version)

type SrcDir = FilePath
type DstDir = FilePath
type SrcPath = FilePath
type DstPath = FilePath

-- default source and destination directories
defSrcDir = "test/md/"
defDstDir = "test/html/"

-- options defined for each program mode
data PandaOptions = 
  Glossary {
    srcdir :: FilePath,
    dstdir :: FilePath
  }
  |
  Build {
    clearance :: Int,
    printables :: Bool
  } deriving (Show, Data, Typeable)

-- glossary options
glossary :: PandaOptions
glossary = Glossary {
    srcdir = defSrcDir &= help "Source directory" &= opt defSrcDir &= typDir,
    dstdir = defDstDir &= help "Target directory" &= opt defDstDir &= typDir
  } 
  &= details [
    "Glossary:",
    "Build the glossary"
  ]

-- build options
build :: PandaOptions
build = Build {
    clearance = 0 &= help "Clearance level",
    printables = False &= help "Generate printables"
  }

pandaOptions :: Mode (CmdArgs PandaOptions)
pandaOptions = cmdArgsMode $ modes [glossary, build]
  &= verbosityArgs [explicit, name "Verbose", name "V"] []
  &= versionArg [explicit, name "version", name "v", programSummary]
  &= programSummary
  &= helpArg [explicit, name "help", name "h"]
  &= program programName

programName = "panda"
programSummary = summary programInfo
programInfo = "panda" ++ showVersion version


optionHandler :: PandaOptions -> IO ()
optionHandler opts@Glossary{..}  = do
    when (null srcdir) $ putStrLn $ "warning: --sourcing markdown from " ++ defSrcDir
    when (null dstdir) $ putStrLn $ "warning: --writing target html to " ++ defDstDir
    exec opts
optionHandler opts@Build{..}  = do
    when (clearance == 0) $ putStrLn "warning: local clearance"
    when (not printables) $ putStrLn "warning: no printables"
    exec opts
 
exec :: PandaOptions -> IO ()
exec opts@Glossary{..} = putStrLn $ "Making glossary, " ++ srcdir ++ " --> " ++ dstdir ++ "."
exec opts@Build{..} = putStrLn $ "Building site at level " ++ show clearance ++ (if printables then " with " else " without ") ++ "printables"

