{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

--
-- NOTE: recode using optional fromJSON constructors
--
data Meta = Object deriving (Generic, Show)

--data Meta = Meta { 
--    alias :: String,              -- Name of the tab which displays this file in HTML page
--    clearance :: Int,             -- clearance level of this file (higher = more public)
--    contributor :: String,           -- Name of contributor(s) 
--    editor :: String,             -- Name of editor(s)
--    source :: String,             -- source to acknowledge 
--    layout :: String,             -- the HTML or TeX id of the layout template
--    keywords :: Array,            -- of keywords
--    year :: String,               -- of a paper
--    paper :: String,              -- name of a paper
--    qno :: Int,                   -- question number
--    stids1 :: Array,              -- of Station ids
--    stids2 :: Array,              -- of Station ids
--    pvids1 :: Array,              -- of Pervasive ids
--    pvids2 :: Array              -- of Pervasive ids
--  } deriving (Generic, Show)

instance FromJSON Meta
instance ToJSON Meta

getMeta :: B.ByteString -> Maybe Meta
getMeta s = decode s

putMeta :: Maybe Meta -> String
putMeta (Just meta) = B.unpack $ encode meta
putMeta _ = "error"





