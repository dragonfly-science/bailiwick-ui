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

data State
 = State Page Adapters 
 deriving (Eq, Show)
data Page 
  = Summary [Area]
  | Home
  deriving (Eq, Show)
data Adapters
  = Adapters
  deriving (Eq, Show)

getPage :: State -> Page
getPage (State page _) = page

getRegion :: State -> Maybe Area
getRegion (State (Summary (reg:_)) _) = Just reg
getRegion _ = Nothing

getSubArea :: State -> Maybe Area
getSubArea (State (Summary (_:subarea:_)) _) = Just subarea
getSubArea _ = Nothing

getArea :: State -> Maybe Area
getArea (State (Summary []) _)    = Nothing
getArea (State (Summary areas) _) = Just $ last areas
getArea (State Home _          )  = Nothing

  

