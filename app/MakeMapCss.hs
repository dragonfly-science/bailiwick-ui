{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
{- Script to make the map.css file, based on the area data -}

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import Data.Text(Text, unpack)
import qualified Data.Text as T


import Bailiwick.Types


main :: IO ()
main = do
  
  args <- getArgs
  let jsonfile
        = case args of
            (a:_) -> a
            _     -> "static/data/areas-1b7549470.json"
  
  -- Read in the data json file
  areas <- readAreas jsonfile

  -- Produce a list of CSS selectors for ta's when zoomed
  putStrLn $ unpack $ T.unlines $ 
      format "div.map.zoom.region.ta g.area[reg=\"taName\"] > path"
          <$> filter (\a -> areaLevel a == "ta") areas

-- div.canterbury.canterbury-zoom.map g.area[reg="Canterbury"] > path

format ::  Text -> Area -> Text
format = flip format'
  where
    format' Area{..}
       = T.replace "ta" areaId
       . T.replace "region" ( maybe "" id $
              listToMaybe [ p | p <- areaParents 
                              , p /= "new-zealand" ])
       . T.replace "taName" areaName



readAreas :: FilePath -> IO [Area]
readAreas file = do
    filecontents <- BS.readFile file
    case eitherDecode filecontents of
        Left err -> error err
        Right vals -> return vals
