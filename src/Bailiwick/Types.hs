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

newtype Indicator = Indicator { indicatorName :: Text }

data AreaSummary
  = AreaSummary
    { areaSummaryId :: Text
    , areaSummaryIndicatorValues :: Object
    } deriving (Eq, Show, Generic)

instance FromJSON AreaSummary where
    parseJSON = withObject "AreaSummary" $ \v -> do
      aid <- v .: "id"
      return $ AreaSummary aid v
    parseJSONList = withObject "AreaSummaries" $ \v -> do
      Array as <- v .: "areaSummaries"
      mapM parseJSON $ V.toList as

type AreaSummaries = OMap Text AreaSummary

mkAreaSummaries :: [ AreaSummary ] -> AreaSummaries
mkAreaSummaries summaries = OMap.fromList [(areaSummaryId a, a) | a <- summaries]


