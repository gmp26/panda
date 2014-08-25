{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Main where

-- we'll use Foldable and Traversable instead of the Prelude defs
import Prelude hiding (
  forM , forM_ , mapM , mapM_ , msum , sequence , sequence_ , 
  concat, elem, notElem)
import  Data.List hiding ( 
  all , and , any , concat , concatMap , elem , filter ,
  find , foldl , foldl' , foldl1 , foldr , foldr1 ,
  mapAccumL , mapAccumR , maximum , maximumBy , minimum , 
  minimumBy , notElem , or , product , sum )
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.Map (Map, toList)
import Data.Maybe (listToMaybe)
import Data.Text (pack, replace, unpack)
import Data.Traversable
import Data.Yaml
import GHC.Generics
import System.Console.GetOpt
import System.Directory (doesFileExist, getDirectoryContents, createDirectoryIfMissing)
import System.Environment
import System.FilePath.Glob (globDir1, compile)
import System.FilePath ((</>), replaceExtension, takeDirectory)
import Text.Pandoc hiding (Meta)
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Panda.Metadata

data OptionFlag = Verbose  | Version
  deriving (Show, Eq)
   
type SrcDir = FilePath
type DstDir = FilePath
type SrcPath = FilePath
type DstPath = FilePath

options :: [OptDescr OptionFlag]
options = 
  [ Option ['V'] ["version"] (NoArg Version)      "show version number",
    Option ['v'] ["verbose"] (NoArg Verbose)      "chatty output on stderr"
  ]

-- | Fetch and parse command line args
processOpts :: [String] -> IO ([OptionFlag], SrcDir, DstDir)
processOpts argv = 
  case getOpt Permute options argv of
     --(o,n,[]  ) -> return (o,n)
     (o,[],[]) -> return (o,"md/","html/")
     (o,srcDir:[],[]) -> return (o,srcDir,"html/")
     (o,srcDir:dstDir:xs,[]) -> return (o,srcDir,dstDir)

     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: md2html [OPTION...] srcdir dstdir"


-- | Apply glob pattern to yield a list of markdown files in a directory
markdownPaths :: FilePath -> IO [FilePath]
markdownPaths = globDir1 (compile "**/*.md")

-- | Return the destination path for a given source path
dstPath :: SrcDir -> DstDir -> SrcPath -> DstPath
dstPath srcDir dstDir = unpack . (replace srcText dstText) . pack . tohtml
  where
    srcText = pack srcDir
    dstText = pack dstDir
    tohtml fp = replaceExtension fp ".html"

-- | Transform a list of source (markdown) paths to a list of dstPath (html) paths
--destPaths :: SrcDir -> DstDir -> [SrcPath] -> [DstPath]
--destPaths srcDir dstDir srcPaths = fmap src2dst srcPaths where
--  src2dst = dstPath srcDir dstDir

-- | Augment a list of source (markdown) paths with dstPath (html) paths
--srcdestPairs :: SrcDir -> DstDir -> [SrcPath] -> [(SrcPath, DstPath)]
--srcdestPairs srcDir dstDir srcPaths = fmap addDestTo srcPaths where
--  src2dst = dstPath srcDir dstDir
--  addDestTo sfp = (sfp, src2dst sfp) 

-- | adjust default pandoc reader options
rOptions :: ReaderOptions
rOptions = def
  { -- The following option causes pandoc to read smart typography, a nice
    -- and free bonus.
    readerSmart = True
  }

-- | adjust default pandoc writer options
wOptions :: WriterOptions
wOptions = def
  {
    writerHtml5 = True,
    writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  }

-- | Run pandoc over a markdown string, making an html string
runPandoc :: String -> String
runPandoc md =
  writeHtmlString wOptions $ readMarkdown rOptions md

-- | Convert the first in a list of markdown source files to html files
--processOne :: SrcDir -> DstDir -> [SrcPath] -> IO ()
--processOne srcDir dstDir [] = return ()
--processOne srcDir dstDir (sfp:sfps) = do
--  input <- readFile sfp
--  let dfp = dstPath srcDir dstDir sfp
--  createDirectoryIfMissing True $ takeDirectory dfp
--  writeFile dfp $ runPandoc input

-- | Convert a list of markdown source files to html files
processAll :: SrcDir -> DstDir -> [SrcPath] -> IO ()
processAll srcDir dstDir [] = return ()
processAll srcDir dstDir (sfp:sfps) = do
  input <- readFile sfp
  let dfp = dstPath srcDir dstDir sfp
  createDirectoryIfMissing True $ takeDirectory dfp
  writeFile dfp $ runPandoc input
  processAll srcDir dstDir sfps


-- | Walk a directory, selecting files, and processing
--mapDir :: (FilePath -> IO ()) -> FilePath -> IO ()
--mapDir proc fp = do
--  isFile <- doesFileExist fp -- is a file of fp
--  if isFile then proc fp     -- process the file
--  else getDirectoryContents fp >>=
--       mapM_ (mapDir proc . (fp </>)) . filter (`notElem` [".", ".."])

main :: IO ()
main = do

  argv <- getArgs
  (userOpts, srcdir, dstdir) <- processOpts argv

  putStrLn $ if Version `elem` userOpts
    then "version 0.1.0.0"
    else ""

  putStrLn $ "srcdir = " ++ srcdir
  putStrLn $ "dstdir = " ++ dstdir

  srcFiles <- markdownPaths srcdir

  if Verbose `elem` userOpts
    then mapM_ putStrLn srcFiles
    else return ()

  processAll srcdir dstdir srcFiles

  -- YAML TEST
  content <- readFile "test/test2.yaml"

  case getEitherMeta (B.pack content) of
    Left s -> putStrLn s
    Right meta -> putStrLn . putMeta $ Just meta  


