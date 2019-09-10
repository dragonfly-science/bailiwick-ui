{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.State
where

import Control.Monad.Fix
import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core
import Language.Javascript.JSaddle.Types (MonadJSM)

import Bailiwick.View.Header (HeaderState(HeaderState))
import Bailiwick.View.Indicators (IndicatorState(IndicatorState))
import Bailiwick.View.ToolBar (ToolBarState(ToolBarState))
import Bailiwick.View.AreaSummary (AreaSummaryState(AreaSummaryState))
import Bailiwick.View.Map (MapState(MapState))
import Bailiwick.View.MapLegend (MapLegendState(MapLegendState))
import Bailiwick.View.IndicatorChart (IndicatorChartState(IndicatorChartState))
import Bailiwick.View.IndicatorSummary (IndicatorSummaryState(IndicatorSummaryState))
import Bailiwick.View.IndicatorTable (IndicatorTableState(IndicatorTableState))
import Bailiwick.Route
import Bailiwick.Store as Store
import Bailiwick.Types

data State t
  = State
    { isSummaryD         :: Dynamic t Bool
    , adaptersD          :: Dynamic t (Set Adapter)
    , store              :: Store t
    , regionD            :: Dynamic t (Maybe Area)
    , areaD              :: Dynamic t (Maybe Area)
    , selectedAreaD      :: Dynamic t (Maybe Area)
    , featureD           :: Dynamic t (Maybe FeatureId)
    , yearD              :: Dynamic t (Maybe Year)
    , indicatorD         :: Dynamic t (Maybe Indicator)
    , indicatorNumbersD  :: Dynamic t IndicatorNumbers
    , chartTypeD         :: Dynamic t (Maybe ChartId)
    , transformD         :: Dynamic t (Maybe TransformId)
    , areaTypeD          :: Dynamic t (Maybe AreaType)
    , compareAreaD       :: Dynamic t (Maybe Area)
    }

route
  :: ( Reflex t
     )
  => State t
  -> Event t Route
route state =
  let routeD = do
        isSummary <- isSummaryD state
        area <- maybe "new-zealand" areaId <$> selectedAreaD state
        compareArea <- compareAreaD state
        adapters <- adaptersD state

        args <- ThemePageArgs
                      <$> (fromMaybe "" . fmap indicatorId <$> indicatorD state)
                      <*> (fromMaybe "" <$> chartTypeD state)
                      <*> (fromMaybe 2019 <$> yearD state)
                      <*> featureD state
                      <*> return Nothing
                      <*> (fromMaybe "" <$> areaTypeD state)
                      <*> (fromMaybe "" <$> transformD state)

        let page = if isSummary
                     then Summary
                     else ThemePage args
        return (Route page area (areaId <$> compareArea) adapters)

  in updated routeD


run
  :: ( Reflex t
     , TriggerEvent t m
     , PerformEvent t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     , MonadHold t m
     , MonadFix m
     )
  => Event t Message
  -> m (State t)
run messageE = do

  store <- Store.run messageE

  isSummaryD <-
    holdDyn False $ fforMaybe messageE $ \case
      Ready (Route Summary _ _ _)  -> Just True
      GoToHomePage                 -> Just True
      GoTo Summary                 -> Just True
      GoTo (ThemePage _)           -> Just False
      _                            -> Nothing

  adaptersD <-
    foldDyn ($) Set.empty $ fforMaybe messageE $ \case
      Ready (Route _ _ _ adapters) -> Just (const adapters)
      SetSubArea _ _               -> Just (Set.insert Mapzoom)
      GoToHomePage                 -> Just (Set.delete Mapzoom)
      ZoomIn                       -> Just (Set.insert Mapzoom)
      ZoomOut _                    -> Just (Set.delete Mapzoom)
      SetShowTable                 -> Just (Set.insert ShowTable)
      UnsetShowTable               -> Just (Set.delete ShowTable)
      _                            -> Nothing

  yearD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route (ThemePage tba) _ _ _) -> Just $ themePageYear tba
      GoTo (ThemePage tba)                -> Just $ themePageYear tba
      SetYear year                        -> Just year
      SetYearArea year _                  -> Just year
      _                                   -> Nothing

  featureD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route (ThemePage tba) _ _ _) -> themePageFeatureId tba
      GoTo (ThemePage tba)                -> themePageFeatureId tba
      SetFeature featureId                -> Just featureId
      _                                   -> Nothing

  chartTypeD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route (ThemePage tba) _ _ _) -> Just $ themePageChartType tba
      GoTo (ThemePage tba)                -> Just $ themePageChartType tba
      SetChartType chartId                -> Just chartId
      _                                   -> Nothing

  transformD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route (ThemePage tba) _ _ _) -> Just $ themePageTransform tba
      GoTo (ThemePage tba)                -> Just $ themePageTransform tba
      SetTransform transform              -> Just transform
      _                                   -> Nothing

  areaTypeD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route (ThemePage tba) _ _ _) -> Just $ themePageAreaType tba
      SetRegion _                         -> Just $ "reg"
      SetSubArea at _                     -> Just at
      GoTo (ThemePage tba)                -> Just $ themePageAreaType tba
      SetAreaType at                      -> Just at
      _                                   -> Nothing

  compareAreaIdD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route _ _ ca _) -> ca
      SetCompareArea ca     -> Just ca
      UnsetCompareArea      -> Nothing
      _                     -> Nothing

  indicatorIdD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route (ThemePage tba) _ _ _) -> Just $ themePageIndicatorId tba
      GoTo (ThemePage tba)                -> Just $ themePageIndicatorId tba
      _                                   -> Nothing

  areaIdD <-
    holdDyn Nothing $ fmap Just $ fforMaybe messageE $ \case
      Ready (Route _ area_id _ _) -> Just area_id
      SetSubArea _ area_id          -> Just area_id
      SetRegion area_id           -> Just area_id
      SetYearArea _ area_id       -> Just area_id
      ZoomOut (Just area_id)      -> Just area_id
      GoToHomePage                -> Just "new-zealand"
      _                           -> Nothing

  let indicatorD = do -- Dynamic t
        mIndicatorId <- indicatorIdD
        mthemes <- storeThemesD store
        return $ do -- Maybe
            themes <- mthemes
            indicatorId <- mIndicatorId
            findIndicator themes indicatorId

      indicatorNumbersD = do
        mindicator <- indicatorD
        indicatorsData <- storeIndicatorsDataD store
        return $ fromMaybe (IndicatorNumbers OMap.empty) $ do
          indid <- indicatorId <$> mindicator
          IndicatorData{indicatorNumbers} <- OMap.lookup indid indicatorsData
          return indicatorNumbers

      regtaD = do
          mareas <- storeAreasD store
          marea_id <- areaIdD
          case (mareas, marea_id) of
            (Just as@(Areas areas), Just route_area) -> do
              let al = areaList as route_area
                  Just nz = OMap.lookup "new-zealand" areas
              case al of
                  [r, t] -> return (Just r, Just t)
                  [r]    -> return (Just r, Nothing)
                  _      -> return (Just nz, Nothing)
            _  -> return (Nothing, Nothing)

      regionD = fst <$> regtaD
      areaD = snd <$> regtaD
      selectedAreaD = zipDynWith (<|>) areaD regionD

      compareAreaD = do
          mareas <- storeAreasD store
          marea_id <- compareAreaIdD
          return $ do
            Areas areas <- mareas
            area_id <- marea_id
            OMap.lookup area_id areas

  return $ State
              { isSummaryD         = isSummaryD
              , adaptersD          = adaptersD
              , store              = store
              , regionD            = regionD
              , selectedAreaD      = selectedAreaD
              , areaD              = areaD
              , featureD           = featureD
              , yearD              = yearD
              , indicatorD         = indicatorD
              , indicatorNumbersD  = indicatorNumbersD
              , chartTypeD         = chartTypeD
              , transformD         = transformD
              , areaTypeD          = areaTypeD
              , compareAreaD       = compareAreaD
              }


-- Header state
makeHeaderState
  :: Reflex t
  => State t -> HeaderState t
makeHeaderState State{isSummaryD,areaD,regionD,yearD,featureD,indicatorD,compareAreaD,store} =
  let areasD = storeAreasD store
  in  HeaderState
        isSummaryD
        regionD
        areaD
        compareAreaD
        yearD
        featureD
        areasD
        indicatorD

-- Indicator state
makeIndicatorState
  :: Reflex t
  => State t -> IndicatorState t
makeIndicatorState State{selectedAreaD,areaTypeD,yearD,indicatorD,store} =
  let themesD = storeThemesD store
  in  IndicatorState
        selectedAreaD
        indicatorD
        yearD
        areaTypeD
        themesD

-- ToolBar State
makeToolBarState
  :: Reflex t
  => State t -> ToolBarState t
makeToolBarState State{chartTypeD,transformD,yearD,areaTypeD,indicatorD} =
  ToolBarState
     indicatorD
     areaTypeD
     transformD
     chartTypeD
     yearD

-- Area Summary state
makeSummaryState
  :: Reflex t
  => State t -> AreaSummaryState t
makeSummaryState State{selectedAreaD,store} =
  let summariesD = fromMaybe OMap.empty <$> storeSummariesD store
      indicatorsD = do
        mthemes <- storeThemesD $ store
        return $ fromMaybe OMap.empty $ do
           themes <- mthemes
           return $ OMap.fromList $ [ (indicatorId i, i)
                                    | i <- concat [ themeIndicators t
                                                  | t <- themes]]
  in  AreaSummaryState
        selectedAreaD
        summariesD
        indicatorsD

-- Map state
makeMapState
  :: Reflex t
  => State t -> MapState t
makeMapState State{areaD,regionD,featureD,transformD,yearD,areaTypeD,indicatorNumbersD,adaptersD,store} =
  let areasD = storeAreasD $ store
  in MapState
      adaptersD
      regionD
      areaD
      areasD
      transformD
      areaTypeD
      featureD
      yearD
      indicatorNumbersD


-- Map Legend state
makeMapLegendState
  :: Reflex t
  => State t -> MapLegendState t
makeMapLegendState State{indicatorD,store,yearD,featureD,transformD,chartTypeD} =
  let inputValuesD = do
        feature <- featureD
        myear <- yearD
        mindicator <- indicatorD
        indicatorsData <- storeIndicatorsDataD store
        return $ do
          year <- myear
          indid <- indicatorId <$> mindicator
          IndicatorData{indicatorScale} <- OMap.lookup indid indicatorsData
          let IndicatorScale scale = indicatorScale
          OMap.lookup (year, feature) scale

  in MapLegendState
        inputValuesD
        yearD
        featureD
        transformD
        chartTypeD
        indicatorD


-- IndicatorChart state
makeIndicatorChartState
  :: Reflex t
  => State t -> IndicatorChartState t
makeIndicatorChartState State{selectedAreaD,
                              featureD,chartTypeD,transformD,yearD,
                              areaTypeD,indicatorD,indicatorNumbersD,
                              store, compareAreaD} =
      IndicatorChartState
         selectedAreaD
         chartTypeD
         featureD
         transformD
         yearD
         areaTypeD
         (storeAreasD store)
         compareAreaD
         indicatorD
         indicatorNumbersD

-- make IndicatorSummaryState
makeIndicatorSummaryState
  :: Reflex t
  => State t -> IndicatorSummaryState t
makeIndicatorSummaryState State{selectedAreaD,featureD,yearD,indicatorD,indicatorNumbersD} =
      IndicatorSummaryState
         selectedAreaD
         (constDyn Nothing)  -- TODO compare area
         featureD            -- feature
         yearD
         indicatorD          -- indicator
         indicatorNumbersD   -- numbers

-- make IndicatorTableState
makeIndicatorTableState
  :: Reflex t
  => State t -> IndicatorTableState t
makeIndicatorTableState State{selectedAreaD,featureD,yearD,indicatorD,indicatorNumbersD,adaptersD} =
     IndicatorTableState
         (hasAdapter ShowTable <$> adaptersD)
         selectedAreaD
         (constDyn Nothing)  -- TODO compare area
         featureD            -- feature
         yearD
         indicatorD          -- indicator
         indicatorNumbersD   -- numbers


findIndicator :: [Theme] -> IndicatorId -> Maybe Indicator
findIndicator themes indid'
  = let indicators = concat $ map themeIndicators $ themes
        loop _ [] = Nothing
        loop indid (i@Indicator{indicatorId}:rest) =
                if indid == indicatorId then Just i else loop indid rest
    in loop indid' indicators


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


