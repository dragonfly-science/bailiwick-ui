{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE NamedFieldPuns          #-}
module Bailiwick.State
where

import Control.Monad (join)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core

import Bailiwick.Route as Route (Route(..), Page(..), ThemePageArgs(..), themePageIndicatorId, getThemePage)
import Bailiwick.Store (Store(..))
import Bailiwick.View.Header (HeaderState(..))
import Bailiwick.View.Indicators (IndicatorState(..))
import Bailiwick.View.ToolBar (ToolBarState(..))
import Bailiwick.Types

data State t
  = Waiting
  | State Page (HeaderState t) (IndicatorState t) (ToolBarState t)

make
  :: (Reflex t)
  => Dynamic t Route -> Dynamic t Store -> Dynamic t (State t)
make routeD storeD = do
  store <- storeD
  route <- routeD
  case store of
    Empty       -> return Waiting
    Loading _ _ -> return Waiting
    Loaded as@(Areas areas) ts -> do

      -- Header state
      let pageD = routePage <$> routeD
          getRegandTa route =
              let al = areaList as (routeArea route)
                  Just nz = OMap.lookup "new-zealand" areas
              in case al of
                  [r, t] -> (r, Just t)
                  [r]    -> (r, Nothing)
                  []     -> (nz, Nothing)
          reg = fst . getRegandTa <$> routeD
          mta = snd . getRegandTa <$> routeD
          header_state = HeaderState pageD reg mta as

      -- Indicator state
      let area = zipDynWith fromMaybe reg mta
          indId = fmap themePageIndicatorId . Route.getThemePage <$> routeD
          indicator_state = IndicatorState area indId ts

      -- ToolBar State
      let mthemepage = Route.getThemePage <$> routeD
          mindicator = join . fmap (findIndicator ts) <$> mthemepage
          toolbar_state = ToolBarState mthemepage mindicator

      return $ State (routePage route) header_state indicator_state toolbar_state

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


getPage :: State t -> Page
getPage (State page _ _ _) = page
getPage _ = Summary

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
