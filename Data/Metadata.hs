{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Metadata 
(
  getMeta ,
  putMeta
) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, toList)
import Data.Maybe
import Data.Yaml
import Data.Vector as V
import GHC.Generics

--
-- NOTE: recode using optional fromJSON constructors
--
data Meta = Meta { 
    alias :: String,              -- Name of the tab which displays this file in HTML page
    clearance :: Int,             -- clearance level of this file (higher = more public)
    contributor :: String,        -- Name of contributor 
    editor :: String,             -- Name of editor
    source :: String,             -- source to acknowledge 
    layout :: String,             -- the HTML or TeX id of the layout template
    keywords :: Array,            -- of keywords
    year :: String,               -- of a paper
    paper :: String,              -- name of a paper
    qno :: Int,                   -- question number
    stids1 :: Array,              -- of Station ids
    stids2 :: Array,              -- of Station ids
    pvids1 :: Array,              -- of Pervasive ids
    pvids2 :: Array               -- of Pervasive ids
  } deriving Show

instance FromJSON Meta where
  parseJSON :: FromJSON a => Value -> Parser a
  parseJSON (Object v) = Meta <$>
                            v .:? "alias" .!= "" <*>
                            v .:? "clearance" .!= 0 <*>
                            v .:? "contributor" .!= "" <*>
                            v .:? "editor" .!= "" <*>
                            v .:? "source" .!= "" <*>
                            v .:? "layout" .!= "" <*>
                            v .:? "keywords" .!= V.empty <*>
                            v .:? "year" .!= "" <*>
                            v .:? "paper" .!= "" <*>
                            v .:? "qno" .!= 0 <*>
                            v .:? "stids1" .!= V.empty <*>
                            v .:? "stids2" .!= V.empty <*>
                            v .:? "pvids1" .!= V.empty <*>
                            v .:? "pvids2" .!= V.empty

  -- A non-Object value is of the wrong type, so use mzero to fail.
  parseJSON _          = mzero

instance ToJSON Meta where
  toJSON :: ToJSON a => a -> Value
  toJSON m = object [
            "alias" .= alias m,
            "clearance" .= clearance m,
            "contributor" .= contributor m,
            "editor" .= editor m,
            "source" .= source m,
            "layout" .= layout m,
            "keywords" .= keywords m,
            "year" .= year m,
            "paper" .= paper m,
            "qno" .= qno m,
            "stids1" .= stids1 m,
            "stids2" .= stids2 m,
            "pvids1" .= pvids1 m,
            "pvids2" .= pvids2 m  
          ]

getMeta :: B.ByteString -> Maybe Meta
getMeta = decode

getEitherMeta :: B.ByteString -> Either String Meta
getEitherMeta = decodeEither

putMeta :: Maybe Meta -> String
putMeta (Just meta) = B.unpack $ encode meta
putMeta _ = "error"





