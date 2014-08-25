{-# LANGUAGE DeriveDataTypeable #-}
module Panda.Options where

import System.Console.CmdArgs
import System.FilePath ()
import Data.Version (showVersion)
import Paths_panda (version)

type SrcDir = FilePath
type DstDir = FilePath
type SrcPath = FilePath
type DstPath = FilePath

data PandaOptions = PandaOptions {
  srcdir :: FilePath,
  dstdir :: FilePath
} deriving (Show, Data, Typeable)

data PandaModes = PandaModes Build | Glossary deriving (Data)

pandaOptions = cmdArgsMode $ PandaOptions {
    srcdir = "test/md/" &= help "Source directory" &= opt "test/md/" &= typDir,
    dstdir = "test/html/" &= help "Target directory" &= opt "test/html/" &= typDir
  } 
  &= summary ("panda-" ++ showVersion version)
  &= program "panda"
