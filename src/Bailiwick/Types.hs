{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Bailiwick.Types where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Aeson
import Data.Aeson.Types (FromJSONKeyFunction(FromJSONKeyText))
import Data.Char as Char

import Data.Scientific (Scientific)
import Data.Hashable (Hashable)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Vector as V

import GHC.Generics

capitalize :: Text -> Text
capitalize inp
 = case Text.uncons inp of
     Nothing    -> ""
     Just (h,t) -> Text.cons (Char.toUpper h) t

type AreaId = Text
data Area
  = Area
    { areaId       :: AreaId
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

newtype Areas
  = Areas { unAreas :: InsOrdHashMap Text Area }
  deriving (Eq, Show, Generic)
instance FromJSON Areas where
    parseJSON v = do
      areas <- parseJSON v
      return $ Areas $ OMap.fromList [(areaId a, a) | a <- areas]

type Year = Integer
data ValueYear = ValueYear Scientific Year
  deriving (Eq, Show, Generic)
instance FromJSON ValueYear where
    parseJSON = withObject "ValueYear" $ \v -> do
        ValueYear <$> v .: "Value" <*> v .: "Year"

type AreaSummary   = InsOrdHashMap IndicatorId (Maybe [ValueYear])
type AreaSummaries = InsOrdHashMap AreaId AreaSummary

data Theme
  = Theme
    { themeId :: Text
    , themeName :: Text
    , themeIndicators :: [ Indicator ]
    } deriving (Eq, Show, Generic)

themeOptions :: Options
themeOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 5 }

instance FromJSON Theme where
    parseJSON = genericParseJSON themeOptions
    parseJSONList = withObject "Themes" $ \v -> do
      Array as <- v .: "themes"
      mapM parseJSON $ V.toList as

type Themes = InsOrdHashMap Text Theme

mkThemes :: [ Theme ] -> Themes
mkThemes themes = OMap.fromList [(themeId t, t) | t <- themes]

data ValueType
  = Quantity
  | Change
  | Aggregate
  deriving (Show, Eq, Generic)

instance FromJSON ValueType where
    parseJSON = genericParseJSON $ defaultOptions{constructorTagModifier = map toLower}

data Units
  = Dollars
  | MillionDollars
  | Percentage
  | Count
  | Hectares
  | Float
  deriving (Show, Eq, Generic)

instance FromJSON Units where
    parseJSON = genericParseJSON $ defaultOptions{constructorTagModifier = map toLower}

data SecondaryNumber
  = SecondaryNumber
  { secNumCaption   :: Text
  , secNumTransform :: Text
  } deriving (Eq, Show, Generic)

secNumOptions :: Options
secNumOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 6 }

instance FromJSON SecondaryNumber where
    parseJSON = genericParseJSON secNumOptions

data Chart
  = Chart
  { chartType        :: Text
  , chartTitle       :: Text
  , chartTransforms2 :: [Transform]
  , chartTransforms  :: [Text]
  , chartFacets      :: Maybe [Text]
  , chartOrder       :: Maybe [Text]
  , chartAxis        :: Maybe [Text]
  , chartMapping     :: Maybe [ChartMapping]
  } deriving (Show, Eq, Generic)

chartOptions :: Options
chartOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 5 }

instance FromJSON Chart where
    parseJSON = genericParseJSON chartOptions

data ChartMapping
  = ChartMapping
  { mappingLabel      :: Text
  , mappingDimensions :: (Text, Text)
  } deriving (Show, Eq, Generic)

mappingOptions :: Options
mappingOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 7 }

instance FromJSON ChartMapping where
    parseJSON = genericParseJSON mappingOptions

data Transform =
  Transform
  { transformName      :: Text
  , transformCaption   :: Text
  , transformFormatter :: Maybe Text
  } deriving (Show, Eq, Generic)

transformOptions :: Options
transformOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 9 }

instance FromJSON Transform where
    parseJSON = genericParseJSON transformOptions

data Language
  = Language
    { langFeatureAccessor       :: Text
    , langFeatureLabel          :: Maybe Text
    , langAreaAccessor          :: Text
    , langSubject               :: Text
    , langSubjectShort          :: Text
    , langSubjectQuantity       :: Text
    , langSubjectAccessor       :: Text
    , langSingular              :: Bool
    , langFeatureAsSubjectLabel :: Bool
    , langCaptions              :: Maybe (Map Text Text)
    , langLabels                :: Maybe (Map Text Text)
    }
    deriving (Eq, Show, Generic)

langOptions :: Options
langOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 4 }

instance FromJSON Language where
    parseJSON = genericParseJSON langOptions

newtype IndicatorId = IndicatorId { unIndicatorId :: Text }
   deriving (Eq, Ord, Show, Generic, Hashable, FromJSONKey, FromJSON)
newtype ChartId = ChartId Text deriving (Eq, Ord, Show, Generic)
instance FromJSON ChartId where
   parseJSON v = ChartId <$> parseJSON v

data Indicator = Indicator
  { indicatorId                     :: IndicatorId
  , indicatorName                   :: Text
--  , indicatorBarchartLabelWidth     :: Maybe Int
  , indicatorAbsoluteLabel          :: Maybe Text
--  , indicatorCaptions               :: Maybe (Map Text Text)
--  , indicatorCharts                 :: [Chart]
  , indicatorDefaultChartLeft       :: ChartId
  , indicatorDefaultChartRight      :: ChartId
--  , indicatorDetailName             :: Maybe Text
--  , indicatorDetails                :: [Text]
--  , indicatorEnableAreaToggle       :: Bool
--  , indicatorFeatureName            :: Maybe Text
  , indicatorFeatures               :: [Text]
--  , indicatorFeatureText            :: Maybe (Map FeatureId Text)
--  , indicatorFeatureDropdownLabel   :: Maybe Text
--  , indicatorFirstYear              :: Text
--  , indicatorHeaderTitle            :: Text
--  , indicatorHeadlineNumCaption     :: Text
--  , indicatorIcon                   :: Maybe Text
--  , indicatorLabels                 :: Maybe (Map Text Text)
--  , indicatorLocalNumCaption        :: Text
--  , indicatorMaxFeatures            :: Maybe (Map Text Text)
--  , indicatorNationalNumCaption     :: Text
--  , indicatorNotes                  :: [Text]
--  , indicatorNz                     :: Text
--  , indicatorPeriod                 :: Maybe Int
--  , indicatorPrimaryYear            :: Maybe Text
--  , indicatorPublishers             :: Text
--  , indicatorRegions                :: [Text]
--  , indicatorScale                  :: Maybe Text
--  , indicatorSlices                 :: [Text]
--  , indicatorSummaryTitle           :: Text
--  , indicatorTerritorialAuthorities :: [Text]
--  , indicatorTooltipExtra           :: Maybe Text
--  , indicatorThemes                 :: [Text]
--  , indicatorTopDetailLabel         :: Maybe Text
--  , indicatorTopFeatureLabel        :: Maybe Text
--  , indicatorUnits                  :: Units
--  , indicatorYearEndMonth           :: Maybe Text
--  , indicatorYears                  :: [Text]
--  , indicatorFeatureTrees           :: [Text]
--  , indicatorAreaTrees              :: [Text]
--  , indicatorSummaries              :: [Text]
--  , indicatorTimeseries             :: [Text]
--  , indicatorMapdata                :: [Text]
--  , indicatorTableRawData           :: [Text]
--  , indicatorTransforms             :: [Transform]

--  , indicatorThemes               :: [Text]
--  , indicatorHeaderTitle          :: Text
--  , indicatorSummaryTitle         :: Text
--  , indicatorHeadlineNumCaption   :: Text
--  , indicatorLocalNum             :: SecondaryNumber
--  , indicatorNationalNum          :: SecondaryNumber
--  , indicatorUnits                :: Units
--  , indicatorValueType            :: ValueType
--  , indicatorCharts               :: [Chart]
--  , indicatorPeriod               :: Maybe Int
--  , indicatorPrimaryYear          :: Maybe Text
--  , indicatorFeatureName          :: Maybe Text
--  , indicatorFeatureDropdownLabel :: Maybe Text
--  , indicatorTopFeatureLabel      :: Maybe Text
--  , indicatorDetailName           :: Maybe Text
--  , indicatorTopDetailLabel       :: Maybe Text
--  , indicatorLeftChart            :: Maybe Text
--  , indicatorLanguageConfig       :: Language
--  , indicatorRightChart           :: Maybe Text
--  , indicatorTooltipExtra         :: Maybe Text
--  , indicatorIcon                 :: Maybe Text
--  , indicatorNotes                :: Maybe [Text]
--  , indicatorSource               :: Text
--  , indicatorScale                :: Maybe Text
--  , indicatorBarchartLabelWidth   :: Maybe Int
--  , indicatorBarChartTicks        :: Maybe Int
--  , indicatorNoTA                 :: Maybe Bool
--  , indicatorNoRegions            :: Maybe Bool
--  , indicatorFeatureOrder         :: Maybe [Text]
--  , indicatorFeatureText          :: Maybe (Map Text Text)
  } deriving (Eq, Show, Generic)

indicatorOptions :: Options
indicatorOptions = defaultOptions
    { fieldLabelModifier = (\case
        [] -> []
        (x:xs) -> toLower x : xs) . drop 9 }

instance FromJSON Indicator where
    parseJSON = genericParseJSON indicatorOptions

type Indicators = InsOrdHashMap IndicatorId Indicator

newtype FeatureId = FeatureId { featureIdText :: Text } deriving (Eq, Ord, Show, Generic)
instance Hashable FeatureId
instance FromJSON FeatureId where
   parseJSON v = FeatureId <$> parseJSON v
instance FromJSONKey FeatureId where
   fromJSONKey = FromJSONKeyText FeatureId

data Feature = Feature
  { featureId     :: FeatureId
  , featureName   :: Text
  , featureParent :: Maybe Text
  } deriving (Eq, Show, Generic)

featureOptions :: Options
featureOptions = defaultOptions
    { fieldLabelModifier = (\case
        [] -> []
        (x:xs) -> toLower x : xs) . drop 7 }

instance FromJSON Feature where
    parseJSON = genericParseJSON featureOptions
    parseJSONList = withObject "Features" $ \v -> do
      Array as <- v .: "features"
      mapM parseJSON $ V.toList as

type Features = InsOrdHashMap FeatureId Feature

mkFeatures :: [ Feature ] -> Features
mkFeatures features = OMap.fromList [(featureId i, i) | i <- features]


-- data FeatureTree = FB { name :: Text, children :: [FeatureTree] }
--                  | FL { name           :: Text
--                        , slug           :: Text
--                        , absolute       :: Double
--                        , dispAbsolute   :: Text
--                        , percentage     :: Double
--                        , dispPercentage :: Text }
--                   deriving (Show, Eq)
--
-- data MapData = MapData
--   { mapId :: Text
--   , mapData :: [ MapValue ]
--   } deriving (Eq, Show, Generic)
--
-- data MapValue = MapValue
--   { mapValAreaId :: Text
--   , mapValAreaName :: Text
--   , mapValDispPercentage :: Double
--   , mapValDispAbsolute :: Text
--   } deriving (Show, Eq, Generic)

newtype AreaName = AreaName { areaNameText :: Text } deriving (Eq, Ord, Show, Generic)
instance Hashable AreaName

data AreaSummaryDisplay = AreaSummaryDisplay
  { areaSlugName :: Text
  , areaPercentage :: Text
  , areaDisplayValue :: Text
  , areaRawValue :: Text
  } deriving (Eq, Show)

data MapSummary = MapSummary
  { mapSummaryId :: Text
  , mapSummaryValues :: Object -- HashMap AreaName AreaSummaryDisplay
  } deriving (Eq, Show)

type MapSummaries = InsOrdHashMap AreaName MapSummary

instance FromJSON MapSummary where
    parseJSON  = withObject "summary" $ \value -> do
        summary <- value .: "summary"
        msid <- summary .: "id"
        rawval <- summary .: "values"
        -- let getmsval (Object hm) =
        --     getmsval v = typemismatch "Expected an object" v
        -- msval <- getmsval rawval
        -- parseJSONList = withObject "Themes" $ \v -> do
        --   Array as <- v .: "themes"
        --   mapM parseJSON $ V.toList as
        return (MapSummary msid rawval)

data ChartData = ChartData
  { chartDataID :: ChartDataName
  , chartDataValues :: Array
  } deriving (Eq, Show, Generic)

newtype ChartDataName = ChartDataName { chartDataNameText :: Text } deriving (Eq, Show, Generic, FromJSON)
instance Hashable ChartDataName

type ChartDatas = InsOrdHashMap ChartDataName ChartData

instance FromJSON ChartData where
  parseJSON = withObject "chartdatum" $ \value -> do
      chartdata <- value .: "chartdatum"
      chartId <- chartdata .: "id"
      chartVals <- chartdata .: "values"

      return (ChartData chartId chartVals)


