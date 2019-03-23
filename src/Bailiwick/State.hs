{-# LANGUAGE OverloadedStrings       #-}
module Bailiwick.State
where

import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core

import Bailiwick.Route as Route (Route(..), Page(..), ThemePageArgs, themePageIndicatorId, getThemePage)
import Bailiwick.Store (Store(..))
import Bailiwick.View.Header (HeaderState(..))
import Bailiwick.View.Indicators (IndicatorState(..))
import Bailiwick.Types

data State t
  = Waiting
  | State (HeaderState t) (IndicatorState t)

make
  :: (Reflex t)
  => Dynamic t Route -> Dynamic t Store -> Dynamic t (State t)
make routeD storeD = do
  store <- storeD
  case store of
    Empty       -> return Waiting
    Loading _ _ -> return Waiting
    Loaded as@(Areas areas) ts -> do

      -- Header state
      let page = routePage <$> routeD
          getRegandTa route =
              let al = areaList as (routeArea route)
                  Just nz = OMap.lookup "new-zealand" areas
              in case al of
                  [r, t] -> (r, Just t)
                  [r]    -> (r, Nothing)
                  []     -> (nz, Nothing)
          reg = fst . getRegandTa <$> routeD
          mta = snd . getRegandTa <$> routeD
          header_state = HeaderState page reg mta as

      -- Indicator state
      let area = zipDynWith fromMaybe reg mta
          indId = fmap themePageIndicatorId . Route.getThemePage <$> routeD
          indicator_state = IndicatorState area indId ts

      return $ State header_state indicator_state



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

getPage :: State t -> Page
getPage _ = Summary

getThemePage :: State t -> Maybe ThemePageArgs
getThemePage _ = Nothing

getIndicators = undefined

getThemes = undefined

stateArea = undefined

getAreaTrees = undefined

getChartData = undefined

getFeatures = undefined

stateCompareArea :: State t -> Maybe Area
stateCompareArea = undefined
