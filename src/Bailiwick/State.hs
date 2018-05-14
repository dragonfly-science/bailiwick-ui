{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.State
where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Bailiwick.Types

-- The application has two parts of the state:
--   1. The selected indicator, area, year, feature etc.,
--   2. The selected presentation.
-- The state is held in a single dynamic at the top level.


data Message 
  = SetRegion Text

data State 
  = Summary
    { summaryRegion  :: Text
    }
  | Home
 deriving (Eq, Show)


mkRegions :: [Area] -> [(Text,Text)]
mkRegions as = ("new-zealand", "New Zealand") :
               [ (areaId a, areaName a) 
               | a <- as
               , areaLevel a == "reg" ]  
getArea :: State -> Text
getArea (Summary area) = area
getArea Home = "new-zealand"

findArea :: Text -> [Area] -> Maybe Area
findArea area areas = find ((area ==) . areaId) areas
  

selectTa :: Text -> [Area] -> Maybe Text
selectTa area areas = do
    thisArea <- findArea area areas
    if areaLevel thisArea `elem` ["ta", "ward"]
        then Just area
        else Nothing

mkTas :: Maybe Text -> [Area] -> [(Text, Text)]
mkTas maybeReg areas = fromMaybe [] $ do
    reg <- maybeReg
    thisArea <- findArea reg areas
    return $ [ (areaId a, areaName a)
             | a <- areas
             , areaId a `elem` areaChildren thisArea ]
    
    
                
