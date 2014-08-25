{-# LANGUAGE DeriveDataTypeable #-}
module Panda.Options where

import System.Console.CmdArgs

data PandaOptions = PandaOptions 
  {
  hello :: String
  } deriving (Show, Data, Typeable)

pandaOptions = cmdArgsMode $ PandaOptions{
  hello = def &= help "World argument" &= opt "world"
  } &= summary "PandaOptions v1"
