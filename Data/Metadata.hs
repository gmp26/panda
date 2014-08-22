{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Metadata 
(
  getMeta ,
  getEitherMeta ,
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
    alias :: Maybe String,        -- Name of the tab which displays this file in HTML page
    clearance :: Maybe Int,             -- clearance level of this file (higher = more public)
    contributor :: Maybe String,        -- Name of contributor 
    editor :: Maybe String,             -- Name of editor
    source :: Maybe String,             -- source to acknowledge 
    layout :: Maybe String,             -- the HTML or TeX id of the layout template
    keywords :: Maybe Array,            -- of keywords
    year :: Maybe String,               -- of a paper
    paper :: Maybe String,              -- name of a paper
    qno :: Maybe Int,                   -- question number
    stids1 :: Maybe Array,              -- of Station ids
    stids2 :: Maybe Array,              -- of Station ids
    pvids1 :: Maybe Array,              -- of Pervasive ids
    pvids2 :: Maybe Array               -- of Pervasive ids
  } deriving Show

instance FromJSON Meta where
  parseJSON (Object v) = Meta <$>
                            v .:? "alias" .!= Nothing <*>
                            v .:? "clearance" .!= Nothing <*>
                            v .:? "contributor" .!= Nothing <*>
                            v .:? "editor" .!= Nothing <*>
                            v .:? "source" .!= Nothing <*>
                            v .:? "layout" .!= Nothing <*>
                            v .:? "keywords" .!= Nothing <*>
                            v .:? "year" .!= Nothing <*>
                            v .:? "paper" .!= Nothing <*>
                            v .:? "qno" .!= Nothing <*>
                            v .:? "stids1" .!= Nothing <*>
                            v .:? "stids2" .!= Nothing <*>
                            v .:? "pvids1" .!= Nothing <*>
                            v .:? "pvids2" .!= Nothing

  -- A non-Object value is of the wrong type, so use mzero to fail.
  parseJSON _          = mzero

instance ToJSON Meta where
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







