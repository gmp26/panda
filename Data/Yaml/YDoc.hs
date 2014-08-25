{-# LANGUAGE OverloadedStrings #-}
module Data.Yaml.YDoc where

-- Wraps Data.Yaml.YamlLight providing round-trip yaml -> YamlLight -> yaml

-- YamLight can parse yaml text into YamlLight values.
-- It implements show, providing a serialisation of the Haskell YamlLight structure.
-- This module wraps YamlLight, providing a new show function that
-- serialises the YamlLight structure to yaml text.

-- This version builds with ghc-7.8 and cabal-1.18.

import Data.Yaml.YamlLight (parseYaml, YamlLight(..))
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, toList)
import Text.PrettyPrint.HughesPJ (text, (<+>), ($+$), nest, empty, vcat, Doc(..))

-- YDoc wraps YamlLight values, providing a show that generates pretty serialised yaml text
newtype YDoc yval = YDoc {yval :: YamlLight}

instance Show (YDoc yval) where 

  show (YDoc yval) = show $ yamlDoc yval where

    yamlDoc :: YamlLight -> Doc
    -- ^Convert a YamlLight value to a pretty print Doc
    yamlDoc yval =
      case yval of
        (YStr bs)   -> text $ B.unpack bs
        (YSeq ymls) -> vcat $ map ((text "-") <+>) (map yamlDoc ymls)
        (YMap m)    -> vcat $ map yamlKVPair (toList m)
        YNil        -> "NULL"

      where 
        -- convert a YMap key value pair to a pretty doc
        yamlKVPair :: (YamlLight, YamlLight) -> Doc
        yamlKVPair (YStr k, yval@(YSeq s)) = vkeyVal k yval
        yamlKVPair (YStr k, yval@(YMap m)) = vkeyVal k yval
        yamlKVPair (YStr k, yval)          = hkeyVal k yval
        yamlKVPair _ = empty

        keyDoc:: B.ByteString -> Doc
        keyDoc k = text ((B.unpack k) ++ ":")

        -- vertical layout for a key value
        vkeyVal :: B.ByteString -> YamlLight -> Doc
        vkeyVal k yval = (keyDoc k) $+$ nest 4 (yamlDoc yval)

        -- horizontal layout for a key value
        hkeyVal :: B.ByteString -> YamlLight -> Doc
        hkeyVal k yval = (keyDoc k) <+> (yamlDoc yval)

