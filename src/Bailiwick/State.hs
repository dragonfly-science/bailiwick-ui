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
import qualified Data.Set as Set (empty, insert, delete, fromList)

import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core hiding (mapMaybe)
import Language.Javascript.JSaddle.Types (MonadJSM)

import Bailiwick.View.Header (HeaderState(HeaderState))
import Bailiwick.View.Indicators (IndicatorState(IndicatorState))
import Bailiwick.View.ToolBar (ToolBarState(ToolBarState))
import Bailiwick.View.AreaSummary (AreaSummaryState(AreaSummaryState))
import Bailiwick.Route
import Bailiwick.Store as Store
import Bailiwick.Types

data State t
  = State
    { isSummaryD         :: Dynamic t Bool
    , adaptersD          :: Dynamic t (Set Adapter)
    , store              :: Store t
    , regionD            :: Dynamic t (Loadable Area)
    , areaD              :: Dynamic t (Loadable Area)
    , selectedAreaD      :: Dynamic t (Loadable Area)
    , featureD           :: Dynamic t (Maybe FeatureId)
    , yearD              :: Dynamic t (Maybe Year)
    , indicatorD         :: Dynamic t (Loadable Indicator)
    , indicatorNumbersD  :: Dynamic t (Loadable IndicatorNumbers)
    , chartTypeD         :: Dynamic t (Maybe ChartId)
    , transformD         :: Dynamic t (Maybe TransformId)
    , areaTypeD          :: Dynamic t (Maybe AreaType)
    , compareAreaD       :: Dynamic t (Loadable (Maybe Area))
    , themesD            :: Dynamic t (Loadable [Theme])
    , areasD             :: Dynamic t (Loadable Areas)
    }

route
  :: ( Reflex t
     )
  => State t
  -> Event t Route
route state =
  let routeD = do
        isSummary <- isSummaryD state
        area <- load "new-zealand" areaId <$> selectedAreaD state
        compareArea <- compareAreaD state
        adapters <- adaptersD state

        args <- ThemePageArgs
                      <$> (fromLoadable "" . fmap indicatorId <$> indicatorD state)
                      <*> (fromMaybe "" <$> chartTypeD state)
                      <*> (fromMaybe 2019 <$> yearD state)
                      <*> featureD state
                      <*> return Nothing
                      <*> (fromMaybe "" <$> areaTypeD state)
                      <*> (fromMaybe "" <$> transformD state)

        let page = if isSummary
                     then Summary
                     else ThemePage args
        return (Route page area (load Nothing (fmap areaId) compareArea) adapters)

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
      ZoomIn _                     -> Just (Set.insert Mapzoom)
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
      ZoomIn (Just at)                    -> Just at
      GoTo (ThemePage tba)                -> Just $ themePageAreaType tba
      SetAreaType at                      -> Just at
      _                                   -> Nothing

  compareAreaIdD <-
    holdDyn Loading $ fforMaybe messageE $ \case
      Ready (Route _ _ mca _)             -> Just (Loaded mca)
      SetCompareArea ca                   -> Just (Loaded (Just ca))
      UnsetCompareArea                    -> Just (Loaded Nothing)
      _                                   -> Nothing

  indicatorIdD <-
    holdDyn Loading $ fforMaybe messageE $ \case
      Ready (Route (ThemePage tba) _ _ _) -> Just $ Loaded $ themePageIndicatorId tba
      GoTo (ThemePage tba)                -> Just $ Loaded $ themePageIndicatorId tba
      Ready (Route Summary _ _ _)         -> Just Missing
      GoToHomePage                        -> Just Missing
      GoTo Summary                        -> Just Missing
      _                                   -> Nothing

  areaIdD <-
    holdDyn Loading $ fforMaybe messageE $ \case
      Ready (Route _ area_id _ _)         -> Just (Loaded area_id)
      SetSubArea _ area_id                -> Just (Loaded area_id)
      SetRegion area_id                   -> Just (Loaded area_id)
      SetYearArea _ area_id               -> Just (Loaded area_id)
      ZoomOut (Just area_id)              -> Just (Loaded area_id)
      GoToHomePage                        -> Just (Loaded "new-zealand")
      _                                   -> Nothing

  let themesD = storeThemesD store

      indicatorD = do -- Dynamic t
        lIndicatorId <- indicatorIdD
        lthemes <- themesD
        return $ do -- Loadable
          themes <- lthemes
          indicatorId <- lIndicatorId
          findIndicator themes indicatorId

      indicatorNumbersD = do
        lindicator <- indicatorD
        indicatorsData <- storeIndicatorsDataD store
        return $ do
          indid <- indicatorId <$> lindicator
          let indicatorData = fromMaybe Missing $ OMap.lookup indid indicatorsData
          IndicatorData{indicatorNumbers} <- indicatorData
          return indicatorNumbers

      areasD = storeAreasD store

      regtaD = do
          lareas <- areasD
          larea_id <- areaIdD
          case (lareas, larea_id) of
            (Loaded as@(Areas areas), Loaded route_area) -> do
              let al = areaList as route_area
                  Just nz = OMap.lookup "new-zealand" areas
              case al of
                  [r, t] -> return (Loaded r, Loaded t)
                  [r]    -> return (Loaded r, Missing)
                  _      -> return (Loaded nz, Missing)
            _  -> return (Loading, Loading)

      regionD = fst <$> regtaD
      areaD = snd <$> regtaD
      selectedAreaD = zipDynWith (<|>) areaD regionD

      compareAreaD = do
          lareas <- areasD
          lmarea_id <- compareAreaIdD
          return $ do
            Areas areas <- lareas
            marea_id <- lmarea_id
            return $ do
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
              , themesD            = themesD
              , areasD             = areasD
              }

-- Areas where there is data for the selected indicator
getDataAreasD
  :: Reflex t
  => State t
  -> Dynamic t (Loadable (Set AreaId))
getDataAreasD State{indicatorNumbersD} = do
  mnumbers <- indicatorNumbersD
  return $ do
    numbers <- mnumbers
    let getAreaId (a, _, _) = a
    let IndicatorNumbers ns = numbers
    return $ Set.fromList $ fmap getAreaId $ OMap.keys ns


-- Header state
makeHeaderState
  :: Reflex t
  => State t -> HeaderState t
makeHeaderState st@State{isSummaryD,areaD,regionD,yearD,featureD,indicatorD,compareAreaD,areasD} =
  HeaderState
        isSummaryD
        (toMaybe <$> regionD)
        (toMaybe <$> areaD)
        (load Nothing id <$> compareAreaD)
        yearD
        featureD
        (toMaybe <$> areasD)
        (toMaybe <$> indicatorD)
        (toMaybe <$> getDataAreasD st)

-- Indicator state
makeIndicatorState
  :: Reflex t
  => State t -> IndicatorState t
makeIndicatorState State{selectedAreaD,areaTypeD,yearD,indicatorD,themesD} =
  IndicatorState
        (toMaybe <$> selectedAreaD)
        (toMaybe <$> indicatorD)
        yearD
        areaTypeD
        (toMaybe <$> themesD)

-- Valid area types for the selected indicator
getAreaTypesD
  :: Reflex t
  => State t
  -> Dynamic t (Maybe (Set AreaType))
getAreaTypesD State{indicatorD,store} = do
  mindicator <- (toMaybe <$> indicatorD)
  indicatorsData <- storeIndicatorsDataD store
  let getAreaType (_, a, _) = a
  return $ do
    indid <- indicatorId <$> mindicator
    inddata <- OMap.lookup indid indicatorsData
    IndicatorData{indicatorScale} <- toMaybe inddata
    let IndicatorScale scale = indicatorScale
        areaTypes = getAreaType <$> (OMap.keys scale)
    return $ Set.fromList areaTypes

-- ToolBar State
makeToolBarState
  :: Reflex t
  => State t -> ToolBarState t
makeToolBarState st@State{chartTypeD,transformD,yearD,areaTypeD,indicatorD} =
  ToolBarState
     (toMaybe <$> indicatorD)
     areaTypeD
     transformD
     chartTypeD
     yearD
     (getAreaTypesD st)

getSummariesD
  :: Reflex t
  => State t
  -> Dynamic t AreaSummaries
getSummariesD State{store} = fromLoadable OMap.empty <$> storeSummariesD store

getIndicatorsD
  :: Reflex t
  => State t
  -> Dynamic t Indicators
getIndicatorsD State{themesD} = do -- Dynamic t
  lthemes <- themesD
  return $ fromLoadable OMap.empty $ do -- Loadable
     themes <- lthemes
     return $ OMap.fromList $ [ (indicatorId i, i)
                              | i <- concat [ themeIndicators t
                                            | t <- themes]]

-- Area Summary state
makeSummaryState
  :: Reflex t
  => State t -> AreaSummaryState t
makeSummaryState st@State{selectedAreaD} =
  AreaSummaryState
        (toMaybe <$> selectedAreaD)
        (getSummariesD st)
        (getIndicatorsD st)

getScaleExtentD
  :: Reflex t
  => State t
  -> Dynamic t (Maybe (Double, Double))
getScaleExtentD State{featureD,yearD,areaTypeD,indicatorD,store} = do
  feature <- featureD
  myear <- yearD
  mareatype <- areaTypeD
  mindicator <- (toMaybe <$> indicatorD)
  indicatorsData <- storeIndicatorsDataD store
  return $ do
    year <- myear
    areatype <- mareatype
    indid <- indicatorId <$> mindicator
    inddata <- OMap.lookup indid indicatorsData
    IndicatorData{indicatorScale} <- toMaybe inddata
    let IndicatorScale scale = indicatorScale
        scaleareatype = if areatype == "nz" then "reg" else areatype
    OMap.lookup (year, scaleareatype, feature) scale


findIndicator :: [Theme] -> IndicatorId -> Loadable Indicator
findIndicator themes indid'
  = let indicators = concat $ map themeIndicators $ themes
        loop _ [] = Missing
        loop indid (i@Indicator{indicatorId}:rest) =
                if indid == indicatorId then Loaded i else loop indid rest
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


