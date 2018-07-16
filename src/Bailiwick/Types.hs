{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.Types where

import GHC.Generics
import Data.Char
import Data.Map (Map)

import qualified Data.Vector as V
import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OMap
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

type Areas = InsOrdHashMap Text Area

mkAreas :: [ Area ] -> Areas
mkAreas areas = OMap.fromList [(areaId a, a) | a <- areas]

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

type AreaSummaries = InsOrdHashMap Text AreaSummary

mkAreaSummaries :: [ AreaSummary ] -> AreaSummaries
mkAreaSummaries summaries = OMap.fromList [(areaSummaryId a, a) | a <- summaries]

data Theme
  = Theme
    { themeId :: Text
    , themeName :: Text
    , themeIndicators :: [ IndicatorId ]
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

newtype IndicatorId = IndicatorId Text deriving (Eq, Ord, Show, Generic)
instance Hashable IndicatorId
instance FromJSON IndicatorId where
   parseJSON v = IndicatorId <$> parseJSON v
newtype ChartId = ChartId Text deriving (Eq, Ord, Show, Generic)
instance FromJSON ChartId where
   parseJSON v = ChartId <$> parseJSON v

data Indicator = Indicator
  { indicatorId                     :: IndicatorId
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
--  , indicatorFeatures               :: [Text]
--  , indicatorFeatureText            :: Maybe (Map Text Text)
--  , indicatorFeatureDropdownLabel   :: Maybe Text
--  , indicatorFirstYear              :: Text
--  , indicatorHeaderTitle            :: Text
--  , indicatorHeadlineNumCaption     :: Text
--  , indicatorIcon                   :: Maybe Text
--  , indicatorLabels                 :: Maybe (Map Text Text)
--  , indicatorLocalNumCaption        :: Text
--  , indicatorMaxFeatures            :: Maybe (Map Text Text)
  , indicatorName                   :: Text
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
--  , indicatorSlices               :: [Text]
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
--  , indicatorAbsoluteLabel        :: Maybe Text
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
    parseJSONList = withObject "Indicators" $ \v -> do
      Array as <- v .: "indicators"
      mapM parseJSON $ V.toList as

type Indicators = InsOrdHashMap IndicatorId Indicator

mkIndicators :: [ Indicator ] -> Indicators
mkIndicators indicators = OMap.fromList [(indicatorId i, i) | i <- indicators]



