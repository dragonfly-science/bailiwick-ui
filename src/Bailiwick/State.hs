{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.State
where

import Data.Text (Text)

import Bailiwick.Types

-- The application has two parts of the state:
--   1. The selected indicator, area, year, feature etc.,
--   2. The selected presentation.
-- The state is held in a single dynamic at the top level.


data Message
  = SetRegion Text
  | SetSubArea Text
  | SetAreaType Text
  | SetLeftTransform Text
  | SetRightTransform Text
  | SetYear Int
  | GoTo Page
  | GoToHomePage
  | ZoomIn
  | ZoomOut (Maybe Text)
  deriving (Eq, Show)

data State
  = State
  { statePage     :: Page
  , stateArea     :: [Area]
  , stateAdapters :: [Adapter]
  } deriving (Eq, Show)

data ThemePageArgs
  = ThemePageArgs
  { themePageIndicatorId    :: IndicatorId
  , themePageLeftChart      :: ChartId
  , themePageRightChart     :: ChartId
  , themePageYear           :: Int
  , themePageFeatureId      :: Maybe Text
  , themePageDetailId       :: Maybe Text
  , themePageAreaType       :: Text
  , themePageLeftTransform  :: Text
  , themePageRightTransform :: Text
  } deriving (Eq, Show)

data Page
  = Summary
  | ThemePage ThemePageArgs
  deriving (Eq, Show)
data Adapter
  = Mapzoom
  deriving (Eq, Show)

getPage :: State -> Page
getPage (State page _ _) = page

hasAdapter :: Adapter -> State -> Bool
hasAdapter adapter State{..} = adapter `elem` stateAdapters


getRegion :: State -> Maybe Area
getRegion (State _ (reg:_) _) = Just reg
getRegion _ = Nothing

getSubArea :: State -> Maybe Area
getSubArea (State _ (_:subarea:_) _) = Just subarea
getSubArea _ = Nothing

getArea :: State -> Maybe Area
getArea (State _ [] _)    = Nothing
getArea (State _ areas _) = Just $ last areas

getThemePage :: State -> Maybe ThemePageArgs
getThemePage State{statePage = ThemePage args} = Just args
getThemePage _ = Nothing

updateThemePage :: State -> (ThemePageArgs -> ThemePageArgs) -> State
updateThemePage s@State{statePage = ThemePage args} f = s{ statePage = ThemePage $ f args }
updateThemePage s _ = s
