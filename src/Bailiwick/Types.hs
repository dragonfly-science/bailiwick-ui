{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.Types where


import GHC.Generics

import Data.Text (Text)
import Data.Aeson


data Areas
  = Areas
    { areas :: [Area]
    } deriving (Eq, Show, Generic)
instance FromJSON Areas

data Area
  = Area
    { id       :: Text
    , name     :: Text
    , level    :: Text
    , children :: [ Text ]
    , parents  :: [ Text ]
    } deriving (Eq, Show, Generic)
instance FromJSON Area

