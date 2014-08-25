{-#LANGUAGE TemplateHaskell #-}
module Experimental.Lens where

data Arc      = Arc      { _degree   :: Int, _minute    :: Int, _second :: Int } deriving (Show)
data Location = Location { _latitude :: Arc, _longitude :: Arc } deriving (Show)

