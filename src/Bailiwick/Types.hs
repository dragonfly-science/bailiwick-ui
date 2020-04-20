{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Bailiwick.Types where

import Control.Applicative
import Data.Aeson
import Data.Char as Char
import Data.String (IsString)

import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Vector as V

import qualified Language.Javascript.JSaddle as JS
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Types (FromJSString)
#endif

import GHC.Generics

-- Replacement for Maybe type
data Loadable a
  = Loading
  | Loaded a
  | Missing
  deriving (Eq, Show)

isLoaded :: Loadable a -> Bool
isLoaded (Loaded _) = True
isLoaded _ = False

load :: a -> (b -> a) -> Loadable b -> a
load _ f (Loaded x) = f x
load x _ _ = x

fromLoadable :: a -> Loadable a -> a
fromLoadable _ (Loaded x) = x
fromLoadable x _ =x

toMaybe :: Loadable a -> Maybe a
toMaybe (Loaded x) = Just x
toMaybe _ = Nothing

toLoadable :: Maybe a -> Loadable a
toLoadable (Just x) = Loaded x
toLoadable Nothing = Missing

instance Functor Loadable where
  fmap f = \case
    Loaded x -> Loaded (f x)
    Loading  -> Loading
    Missing  -> Missing

instance Applicative Loadable where
  pure = Loaded
  Loaded f <*> lx = fmap f lx
  Missing <*> _ = Missing
  Loading <*> _ = Loading

instance Monad Loadable where
  (Loaded x) >>= k      = k x
  Loading  >>= _      = Loading
  Missing  >>= _      = Missing
  (>>) = (*>)
  fail _              = Missing

instance Alternative Loadable where
  empty = Missing
  Missing <|> r = r
  l       <|> _ = l   -- Loading gets passed through

instance JS.ToJSVal a => JS.ToJSVal (Loadable a) where
  toJSVal (Loaded x) = JS.toJSVal x
  toJSVal Missing = return JS.jsNull
  toJSVal Loading = return JS.jsNull

type AreaId = Text
type AreaType = Text
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

newtype Areas
  = Areas { unAreas :: InsOrdHashMap AreaId Area }
  deriving (Eq, Show, Generic)
instance FromJSON Areas where
    parseJSON v = do
      areas <- parseJSON v
      return $ Areas $ OMap.fromList [(areaId a, a) | a <- areas]

type Year = Int
newtype YearValueDisp = YearValueDisp {unYearValueDisp :: (Year, Double, Text)}
  deriving (Eq, Show, Generic, FromJSON)
type AreaSummary   = InsOrdHashMap AreaId (Maybe [YearValueDisp])
type AreaSummaries = InsOrdHashMap IndicatorId AreaSummary

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
  deriving (Eq, Show, Generic)

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

data Facet
  = Facet
  { facetName :: Text } deriving (Eq, Show, Generic)

facetOptions :: Options
facetOptions = defaultOptions
      { fieldLabelModifier = map toLower . drop 5 }

instance JS.ToJSVal Facet

instance FromJSON Facet where
  parseJSON = genericParseJSON facetOptions

data Chart
  = Chart
  { chartType        :: Text
  , chartTitle       :: Maybe Text
  , chartTransforms2 :: Maybe [Transform]
  , chartTransforms  :: [Transform]
  , chartFacets      :: Maybe [Facet]
  , chartOrder       :: Maybe [Text]
  , chartAxis        :: Maybe [Text]
  , chartMapping     :: Maybe [ChartMapping]
  } deriving (Eq, Show, Generic)

chartOptions :: Options
chartOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 5 }

instance JS.ToJSVal Chart

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

instance JS.ToJSVal ChartMapping
instance FromJSON ChartMapping where
    parseJSON = genericParseJSON mappingOptions

data Transform =
  Transform
  { transformName      :: Maybe Text
  , transformCaption   :: Maybe Text
  , transformFormatter :: Maybe Text
  } deriving (Show, Eq, Generic)

transformOptions :: Options
transformOptions = defaultOptions
    { fieldLabelModifier = map toLower . drop 9 }

instance JS.ToJSVal Transform

instance FromJSON Transform where
    parseJSON = genericParseJSON transformOptions

data Language
  = Language
    { langFeatureAccessor       :: Maybe Text
    , langFeatureLabel          :: Maybe Text
    , langSubject               :: Maybe Text
    , langSubjectShort          :: Maybe Text
    , langSubjectQuantity       :: Maybe Text
    , langSubjectAccessor       :: Maybe Text
    , langSingular              :: Maybe Bool
    , langFeatureAsSubjectLabel :: Maybe Bool
    , langCaptions              :: Maybe (Map Text Text)
    , langLabels                :: Maybe (Map Text Text)
    }
    deriving (Eq, Show, Generic)

langOptions :: Options
langOptions = defaultOptions
    { fieldLabelModifier = (\case
        [] -> []
        (x:xs) -> toLower x : xs) . drop 4 }

instance FromJSON Language where
    parseJSON = genericParseJSON langOptions

newtype IndicatorId = IndicatorId { unIndicatorId :: Text }
   deriving (Eq, Ord, Generic, Hashable,
             FromJSONKey, FromJSON, IsString, JS.ToJSVal)
instance Show IndicatorId where
   show (IndicatorId indid) = Text.unpack indid
newtype ChartId = ChartId { unChartId :: Text }
    deriving (Eq, Ord, Show, Generic, JS.ToJSVal, Hashable, FromJSONKey, IsString)
instance FromJSON ChartId where
   parseJSON v = ChartId <$> parseJSON v

data Indicator = Indicator
  { indicatorId                     :: IndicatorId
  , indicatorName                   :: Text
  , indicatorHeaderTitle            :: Text
  , indicatorSummaryTitle           :: Text
  , indicatorAbsoluteLabel          :: Maybe Text
  , indicatorDefaultChartLeft       :: ChartId
  , indicatorDefaultChartRight      :: ChartId
  , indicatorFeatures               :: [Text]
  , indicatorUnits                  :: Units
  , indicatorValueType              :: ValueType
  , indicatorTopDetailLabel         :: Maybe Text
  , indicatorDefaultFeature         :: Maybe Text
  , indicatorFeatureName            :: Maybe Text
  , indicatorFeatureDropdownLabel   :: Maybe Text
  , indicatorYearEndMonth           :: Maybe Text
  , indicatorFeatureText            :: Maybe (InsOrdHashMap FeatureId Text)
  , indicatorFirstYear              :: Text
  , indicatorPeriod                 :: Maybe Int
  , indicatorNotes                  :: Maybe [Text]
  , indicatorPublishers             :: Text
  , indicatorNationalNumCaption     :: Text
  , indicatorLocalNumCaption        :: Text
  , indicatorHeadlineNumCaption     :: Text
  , indicatorTooltipExtra           :: Maybe Text
  , indicatorYears                  :: [Year]
  , indicatorCaptions               :: InsOrdHashMap Text Text
  , indicatorLabels                 :: InsOrdHashMap Text Text
--  , indicatorBarchartLabelWidth     :: Maybe Int
  , indicatorCharts                 :: Maybe (InsOrdHashMap ChartId Chart)
--  , indicatorDetailName             :: Maybe Text
--  , indicatorDetails                :: [Text]
--  , indicatorEnableAreaToggle       :: Bool
--  , indicatorIcon                   :: Maybe Text
--  , indicatorMaxFeatures            :: Maybe (Map Text Text)
--  , indicatorNz                     :: Text
--  , indicatorPrimaryYear            :: Maybe Text
--  , indicatorRegions                :: [Text]
--  , indicatorScale                  :: Maybe Text
--  , indicatorSlices                 :: [Text]
--  , indicatorTerritorialAuthorities :: [Text]
--  , indicatorTooltipExtra           :: Maybe Text
--  , indicatorThemes                 :: [Text]
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
--  , indicatorPeriod               :: Maybe Int
--  , indicatorPrimaryYear          :: Maybe Text
--  , indicatorFeatureName          :: Maybe Text
--  , indicatorFeatureDropdownLabel :: Maybe Text
--  , indicatorDetailName           :: Maybe Text
--  , indicatorTopDetailLabel       :: Maybe Text
--  , indicatorLeftChart            :: Maybe Text
  , indicatorLanguageConfig       :: Language
--  , indicatorRightChart           :: Maybe Text
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

type TransformId = Text
type DetailId = Text

newtype FeatureId = FeatureId { featureIdText :: Text }
   deriving (Eq, Ord, Show, Generic, Hashable,
             FromJSONKey, FromJSON, IsString, JS.ToJSVal)

instance FromJSONKey (Maybe FeatureId)
instance JS.FromJSVal FeatureId
#ifdef ghcjs_HOST_OS
instance JS.PFromJSVal FeatureId where
    pFromJSVal val = FeatureId (JS.pFromJSVal val)
instance GHCJS.DOM.Types.FromJSString FeatureId
#else
instance JS.FromJSString FeatureId where
    fromJSString  = FeatureId . JS.fromJSString
#endif

data Feature = Feature
  { featureId     :: FeatureId
  , featureName   :: Text
  , featureParent :: Maybe FeatureId
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

newtype AreaName = AreaName { areaNameText :: Text } deriving (Eq, Ord, Show, Generic, JS.ToJSVal)
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


-- Indicator Data
type Colour = Text
type Display = Text
data IndicatorData
  = IndicatorData
    { indicatorIdent    :: IndicatorId
    , indicatorNumbers  :: IndicatorNumbers
    , indicatorScale    :: IndicatorScale
    } deriving (Eq, Show, Generic)
instance FromJSON IndicatorData where
  parseJSON = genericParseJSON options
      where
        options = defaultOptions { fieldLabelModifier = map toLower . drop 9 }

newtype IndicatorNumbers =
  IndicatorNumbers (InsOrdHashMap (AreaId, Year, Maybe FeatureId) Numbers)
  deriving (Eq, Show, Generic)
instance FromJSON IndicatorNumbers where
  parseJSON
    = (IndicatorNumbers . OMap.fromList . V.toList <$>) .
        (withArray "indicatornumbers" $ mapM $
            (withObject "indicatornumbers" $ \value -> do
              areaid   <- value .:  "areaid"
              year     <- value .:  "year"
              feature  <- value .:? "feature"
              headline <- value .:  "headline"
              local    <- value .:  "local"
              national <- value .:  "national"
              raw      <- value .:? "rawvalue"
              rawnatl  <- value .:? "rawnational"
              rawlocal <- value .:? "rawlocal"
              index    <- value .:? "index"
              indexD   <- value .:  "indexDisp"
              return ( (areaid, year, feature)
                     , Numbers
                          { headlineDisp = headline
                          , localDisp    = local
                          , nationalDisp = national
                          , indexDisp    = indexD
                          , rawNum       = raw
                          , localNum     = rawlocal
                          , nationalNum  = rawnatl
                          , indexNum     = index
                          })))

newtype IndicatorScale =
  IndicatorScale (InsOrdHashMap (Year, AreaType, Maybe FeatureId) (Double, Double))
  deriving (Eq, Show, Generic)
instance FromJSON IndicatorScale where
  parseJSON
    = (IndicatorScale . OMap.fromList . V.toList <$>) .
        (withArray "indicatorscale" $ mapM $
            (withObject "indicatorscale" $ \value -> do
              year     <- value .:  "year"
              areatype <- value .:  "areatype"
              feature  <- value .:? "feature"
         --   detail   <- value .:? "detail"
              minval   <- value .:  "minval"
              maxval   <- value .:  "maxval"
              return ( ( year, areatype, feature)
                     , (minval, maxval))))

data Numbers
  = Numbers
    { headlineDisp :: Text
    , localDisp    :: Text
    , nationalDisp :: Text
    , indexDisp    :: Text
    , rawNum       :: Maybe Double
    , localNum     :: Maybe Double
    , nationalNum  :: Maybe Double
    , indexNum     :: Maybe Double
    }
  deriving (Eq, Show, Generic)

loadingNumbers :: Numbers
loadingNumbers = Numbers "..." "..." "..." "..."
                       Nothing Nothing Nothing Nothing
emptyNumbers :: Numbers
emptyNumbers = Numbers "No data" "No data" "No data" "No data"
                       Nothing Nothing Nothing Nothing

newtype ScaleFunction = ScaleFunction JS.Object deriving (Generic)
instance Show ScaleFunction where
    show _ = "Scale function"


