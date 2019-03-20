module Bailiwick.State
where

import Bailiwick.Route (Route(..), Page(..), ThemePageArgs)
import Bailiwick.Store (Store(..))
import Bailiwick.Types

import qualified Data.HashMap.Strict.InsOrd as OMap

data State = State deriving (Show, Eq)

make :: Route -> Store -> State
make = undefined

getRoute :: State -> Route
getRoute = undefined

getArea = undefined

getAreas :: State -> Areas
getAreas _ = OMap.empty

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
