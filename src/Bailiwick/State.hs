{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Bailiwick.State
where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core

import Bailiwick.View.Header (HeaderState(HeaderState))
import Bailiwick.View.Indicators (IndicatorState(IndicatorState))
import Bailiwick.View.ToolBar (ToolBarState(ToolBarState))
import Bailiwick.View.AreaSummary (AreaSummaryState(AreaSummaryState))
import Bailiwick.View.Map (MapState(MapState))
import Bailiwick.View.MapLegend (MapLegendState(MapLegendState))
import Bailiwick.View.IndicatorChart (IndicatorChartState(IndicatorChartState))
import Bailiwick.View.IndicatorSummary (IndicatorSummaryState(IndicatorSummaryState))
import Bailiwick.Route
import Bailiwick.Store
import Bailiwick.Types

data State t
  = State
    { routeD             :: Dynamic t Route
    , store              :: Store t
    , regionD            :: Dynamic t (Maybe Area)
    , areaD              :: Dynamic t (Maybe Area)
    , featureD           :: Dynamic t (Maybe FeatureId)
    , yearD              :: Dynamic t (Maybe Year)
    , indicatorD         :: Dynamic t (Maybe Indicator)
    , indicatorNumbersD  :: Dynamic t IndicatorNumbers
    }

make
  :: (Reflex t)
  => Dynamic t Route -> Store t -> State t
make routeD store@Store{..} =
  let regta = do
          mareas <- storeAreasD
          case mareas of
            Nothing -> return (Nothing, Nothing)
            Just as@(Areas areas) -> do
              route_area <- routeArea <$> routeD
              let al = areaList as route_area
                  Just nz = OMap.lookup "new-zealand" areas
              case al of
                  [r, t] -> return (Just r, Just t)
                  [r]    -> return (Just r, Nothing)
                  _      -> return (Just nz, Nothing)

      mthemepageD = getThemePage <$> routeD
      indicatorD = do -- Dynamic t
        mthemepage <- mthemepageD
        mthemes <- storeThemesD
        return $ do -- Maybe
            themes <- mthemes
            themepage <- mthemepage
            findIndicator themes themepage

      indicatorNumbersD = do
        mindicator <- indicatorD
        indicatorsData <- storeIndicatorsDataD
        return $ fromMaybe (IndicatorNumbers OMap.empty) $ do
          indid <- indicatorId <$> mindicator
          IndicatorData{..} <- OMap.lookup indid indicatorsData
          return indicatorNumbers

      featureD = do
        route <- routeD
        return $ do
          ThemePageArgs{..} <- getThemePage route
          themePageFeatureId

      yearD = do
        route <- routeD
        return $ do
          ThemePageArgs{..} <- getThemePage route
          return themePageYear


  in State
       { routeD             = routeD
       , store              = store
       , regionD            = fst <$> regta
       , areaD              = snd <$> regta
       , featureD           = featureD
       , yearD              = yearD
       , indicatorD         = indicatorD
       , indicatorNumbersD  = indicatorNumbersD
       }


-- Header state
makeHeaderState
  :: Reflex t
  => State t -> HeaderState t
makeHeaderState State{..} =
  let areasD = storeAreasD store
  in  HeaderState routeD regionD areaD featureD areasD indicatorD

-- Indicator state
makeIndicatorState
  :: Reflex t
  => State t -> IndicatorState t
makeIndicatorState State{..} =
  let selectedAreaD = zipDynWith (<|>) areaD regionD
      indId = fmap themePageIndicatorId . getThemePage <$> routeD
  in  IndicatorState routeD selectedAreaD indId (storeThemesD $ store)

-- ToolBar State
makeToolBarState
  :: Reflex t
  => State t -> ToolBarState t
makeToolBarState State{..} =
  let mthemepageD = getThemePage <$> routeD
  in  ToolBarState mthemepageD indicatorD

-- Area Summary state
makeSummaryState
  :: Reflex t
  => State t -> AreaSummaryState t
makeSummaryState State{..} =
  let selectedAreaD = zipDynWith (<|>) areaD regionD
      summariesD = fromMaybe OMap.empty <$> storeSummariesD store
      indicatorsD = do
        mthemes <- storeThemesD $ store
        return $ fromMaybe OMap.empty $ do
           themes <- mthemes
           return $ OMap.fromList $ [ (indicatorId i, i)
                                    | i <- concat [ themeIndicators t
                                                  | t <- themes]]
  in  AreaSummaryState selectedAreaD summariesD indicatorsD

-- Map state
makeMapState
  :: Reflex t
  => State t -> MapState t
makeMapState State{..} =
  MapState routeD regionD areaD (storeAreasD $ store) indicatorNumbersD

-- Map Legend state
makeMapLegendState
  :: Reflex t
  => State t -> MapLegendState t
makeMapLegendState State{..} =
  let scaleD = do
        feature <- featureD
        myear <- yearD
        mindicator <- indicatorD
        indicatorsData <- storeIndicatorsDataD store
        return $ do
          year <- myear
          indid <- indicatorId <$> mindicator
          IndicatorData{..} <- OMap.lookup indid indicatorsData
          let IndicatorScale scale = indicatorScale
          OMap.lookup (year, feature) scale
  in MapLegendState scaleD


-- IndicatorChart state
makeIndicatorChartState
  :: Reflex t
  => State t -> IndicatorChartState t
makeIndicatorChartState State{..} =
  let selectedAreaD = zipDynWith (<|>) areaD regionD
  in IndicatorChartState
          routeD
          selectedAreaD
          (storeAreasD store)
          (constDyn Nothing) -- TO DO Feature
          indicatorD
          indicatorNumbersD


-- make IndicatorSummaryState
makeIndicatorSummaryState
  :: Reflex t
  => State t -> IndicatorSummaryState t
makeIndicatorSummaryState State{..} =
  let selectedAreaD = zipDynWith (<|>) areaD regionD
  in IndicatorSummaryState routeD selectedAreaD
         (constDyn Nothing)  -- TODO compare area
         featureD            -- TODO feature
         indicatorD          -- indicator
         indicatorNumbersD   -- numbers



findIndicator :: [Theme] -> ThemePageArgs -> Maybe Indicator
findIndicator themes ThemePageArgs{themePageIndicatorId}
  = let indicators = concat $ map themeIndicators $ themes
        loop _ [] = Nothing
        loop indid (i@Indicator{indicatorId}:rest) =
                if indid == indicatorId then Just i else loop indid rest
    in loop themePageIndicatorId indicators


areaList :: Areas -> Text -> [Area]
areaList _ "new-zealand" = []
areaList (Areas areas) p = case (area, parent) of
                  (Just a, Just b)  -> [b, a]
                  (Just a, Nothing) -> [a]
                  _                 -> []
  where
    area = OMap.lookup p areas
    parent = do
      a <- area
      -- TODO: handle accessedvia
      listToMaybe
        [ parentArea
        | parentArea <- mapMaybe (`OMap.lookup` areas) (areaParents a)
        , areaLevel parentArea == "reg" ]


getRoute :: State t -> Route
getRoute = undefined


getAreas :: State t -> Areas
getAreas _ = Areas OMap.empty

getAreaSummaries :: State t -> AreaSummaries
getAreaSummaries = undefined

getRegion :: State t -> Maybe Area
getRegion _ = Nothing

getSubArea :: State t -> Maybe Area
getSubArea _ = Nothing




stateCompareArea :: State t -> Maybe Area
stateCompareArea = undefined
