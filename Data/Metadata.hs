{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Metadata 
(
  getMeta ,
  putMeta
) where

import qualified Data.ByteString.Char8 as B
import Data.Map (Map, toList)
import Data.Maybe
import Data.Yaml
import GHC.Generics

data Coord = Coord { x :: Double, y :: Double } deriving Generic

data Obj = Obj {
    foo :: String
} deriving (Generic, Show)
instance FromJSON Obj
instance ToJSON Obj

data Meta = Meta { 
    alias :: String, 
    clearance :: Int, 
    contributor :: Obj, 
    editor :: String
  } deriving (Generic, Show)

instance FromJSON Meta
instance ToJSON Meta

getMeta :: B.ByteString -> Maybe Meta
getMeta s = decode s

--getFoo :: Maybe Meta -> String
--getFoo (Just meta) = foo $ contributor meta

putMeta :: Maybe Meta -> String
putMeta (Just meta) = B.unpack $ encode meta
putMeta _ = "error"





