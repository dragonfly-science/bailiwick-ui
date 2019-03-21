{-# LANGUAGE OverloadedStrings       #-}
module Bailiwick.State
where

import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core

import Bailiwick.Route (Route(..), Page(..), ThemePageArgs)
import Bailiwick.Store (Store(..))
import Bailiwick.Types

data State t
  = Loading
  | State (HeaderState t)

data HeaderState t
  = HeaderState
  { pageD     :: Dynamic t Page
  , areaD     :: Dynamic t Area
  , subareaD  :: Dynamic t (Maybe Area)
  , areas     :: Areas
  }

make
  :: (Reflex t)
  => Dynamic t Route -> Dynamic t Store -> Dynamic t (State t)
make routeD storeD = do
  store <- storeD
  case store of
    Empty -> return Loading
    LoadAreas as@(Areas areas) -> do
      let page = routePage <$> routeD
      let getRegandTa route =
              let al = areaList as (routeArea route)
                  Just nz = OMap.lookup "new-zealand" areas
              in case al of
                  [r, t] -> (r, Just t)
                  [r]    -> (r, Nothing)
                  []     -> (nz, Nothing)
      let reg = fst . getRegandTa <$> routeD
      let mta = snd . getRegandTa <$> routeD
      return $ State $ HeaderState page reg mta as



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
