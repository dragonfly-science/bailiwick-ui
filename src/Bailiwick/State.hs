{-# LANGUAGE OverloadedStrings       #-}
module Bailiwick.State
where

import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

import Data.Text (Text)

import Bailiwick.Route (Route(..), Page(..), ThemePageArgs)
import Bailiwick.Store (Store(..))
import Bailiwick.Types

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OMap

data State
  = Loading
  | State HeaderState
  deriving (Show, Eq)

data HeaderState
  = HeaderState
  { page     :: Page
  , region   :: Area
  , subarea  :: Maybe Area
  , regions  :: InsOrdHashMap Text Area
  , subareas :: InsOrdHashMap Text Area
  }
  deriving (Show, Eq)

make :: Route -> Store -> State
make _ Empty = Loading
make route (LoadAreas as@(Areas areas)) =
  let al = areaList as (routeArea route)
      Just nz = OMap.lookup "new-zealand" areas
      (reg, mta)
         = case al of
             [reg, ta] -> (reg, Just ta)
             [reg]     -> (reg, Nothing)
             []        -> (nz, Nothing)
      regs = 
        let regions = OMap.filter (\a -> areaLevel a == "reg") areas
        in  OMap.singleton "new-zealand" nz <> regions
      tas' = do
        fromMaybe OMap.empty $ do
          thisArea <- OMap.lookup (areaId reg) areas
          return $ OMap.filter (\a -> areaId a `elem` areaChildren thisArea) areas
      page = routePage route
  in  State $ HeaderState page reg mta regs tas'



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



getRoute :: State -> Route
getRoute = undefined

getArea = undefined

getAreas :: State -> Areas
getAreas _ = Areas OMap.empty

getAreaSummaries :: State -> AreaSummaries
getAreaSummaries = undefined

getRegion :: State -> Maybe Area
getRegion _ = Nothing

getSubArea :: State -> Maybe Area
getSubArea _ = Nothing

getPage :: State -> Page
getPage _ = Summary

getThemePage :: State -> Maybe ThemePageArgs
getThemePage _ = Nothing

getIndicators = undefined

getThemes = undefined

stateArea = undefined

getAreaTrees = undefined

getChartData = undefined

getFeatures = undefined

stateCompareArea :: State -> Maybe Area
stateCompareArea = undefined
