{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.Types where


import GHC.Generics
import Data.Char

import qualified Data.Vector as V
import Data.Text (Text)
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import Data.Aeson

data Area
  = Area
    { areaId       :: Text
    , areaName     :: Text
    , areaLevel    :: Text
    , areaChildren :: [ Text ]
    , areaParents  :: [ Text ]
    } deriving (Eq, Show, Generic)

areaOptions :: Options
areaOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 4 }

instance FromJSON Area where
    parseJSON = genericParseJSON areaOptions
    parseJSONList = withObject "Areas" $ \v -> do
      Array as <- v .: "areas"
      mapM parseJSON $ V.toList as

type Areas = OMap Text Area
        
mkAreas :: [ Area ] -> Areas
mkAreas areas = OMap.fromList [(areaId a, a) | a <- areas]


