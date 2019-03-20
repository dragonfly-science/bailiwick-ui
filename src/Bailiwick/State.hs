module Bailiwick.State
where

import Bailiwick.Route (Route(..), Page(..), ThemePageArgs)
import Bailiwick.Store (Store(..))
import Bailiwick.Types

data State = State deriving (Show, Eq)

make :: Route -> Store -> State
make = undefined

getRoute :: State -> Route
getRoute = undefined

getArea = undefined

getAreas = undefined

getAreaSummaries :: State -> AreaSummaries
getAreaSummaries = undefined

getRegion :: State -> Maybe Area
getRegion = undefined

getSubArea :: State -> Maybe Area
getSubArea = undefined

getPage :: State -> Page
getPage = undefined

getThemePage :: State -> Maybe ThemePageArgs
getThemePage = undefined

getIndicators = undefined

getThemes = undefined

stateArea = undefined

getAreaTrees = undefined

getChartData = undefined

getFeatures = undefined

stateCompareArea :: State -> Maybe Area
stateCompareArea = undefined
