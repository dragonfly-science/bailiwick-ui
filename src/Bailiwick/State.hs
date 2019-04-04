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
import Bailiwick.View.IndicatorSummary (IndicatorSummaryState(IndicatorSummaryState))
import Bailiwick.Route
import Bailiwick.Store
import Bailiwick.Types

data State t
  = State
    { routeD  :: Dynamic t Route
    , storeD  :: Dynamic t Store
    , regionD :: Dynamic t (Maybe Area)
    , areaD   :: Dynamic t (Maybe Area)
    }

make
  :: (Reflex t)
  => Dynamic t Route -> Dynamic t Store -> State t
make routeD storeD =
  let regta = do
          mareas <- storeAreas <$> storeD
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
  in State
       { routeD  = routeD
       , storeD  = storeD
       , regionD = fst <$> regta
       , areaD   = snd <$> regta
       }


-- Header state
makeHeaderState
  :: Reflex t
  => State t -> HeaderState t
makeHeaderState State{..} =
  let pageD = routePage <$> routeD
      areasD = storeAreas <$> storeD
  in  HeaderState pageD regionD areaD areasD

-- Indicator state
makeIndicatorState
  :: Reflex t
  => State t -> IndicatorState t
makeIndicatorState State{..} =
  let selectedAreaD = zipDynWith (<|>) areaD regionD
      indId = fmap themePageIndicatorId . getThemePage <$> routeD
  in  IndicatorState selectedAreaD indId (storeThemes <$> storeD)

-- ToolBar State
makeToolBarState
  :: Reflex t
  => State t -> ToolBarState t
makeToolBarState State{..} =
  let mthemepageD = getThemePage <$> routeD
      mindicatorD = do -- Dynamic t
        mthemepage <- mthemepageD
        mthemes <- storeThemes <$> storeD
        return $ do -- Maybe
            themes <- mthemes
            themepage <- mthemepage
            findIndicator themes themepage
  in  ToolBarState mthemepageD mindicatorD

-- Area Summary state
makeSummaryState
  :: Reflex t
  => State t -> AreaSummaryState t
makeSummaryState State{..} =
  let selectedAreaD = zipDynWith (<|>) areaD regionD
      summariesD = fromMaybe OMap.empty . storeSummaries <$> storeD
      indicatorsD = do
        mthemes <- storeThemes <$> storeD
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
  MapState routeD regionD areaD (storeAreas <$> storeD)


-- make IndicatorSummaryState
makeIndicatorSummaryState
  :: Reflex t
  => State t -> IndicatorSummaryState t
makeIndicatorSummaryState State{..} =
  let selectedAreaD = zipDynWith (<|>) areaD regionD
  in IndicatorSummaryState routeD selectedAreaD
         (constDyn Nothing) -- TODO compare area
         (constDyn Nothing) -- TODO indicator
         (constDyn Nothing) -- TODO feature



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

getArea = undefined

getAreas :: State t -> Areas
getAreas _ = Areas OMap.empty

getAreaSummaries :: State t -> AreaSummaries
getAreaSummaries = undefined

getRegion :: State t -> Maybe Area
getRegion _ = Nothing

getSubArea :: State t -> Maybe Area
getSubArea _ = Nothing



getIndicators = undefined

getThemes = undefined

stateArea = undefined

getAreaTrees = undefined

getChartData = undefined

getFeatures = undefined

stateCompareArea :: State t -> Maybe Area
stateCompareArea = undefined
