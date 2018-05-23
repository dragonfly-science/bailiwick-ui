{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
{- Script to make the map.css file, based on the area data -}

import System.Environment (getArgs)
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


import Bailiwick.Types


main :: IO ()
main = do
  
  args <- getArgs
  let (templatefile, areasfile)
        = case args of
            (a:b:_) -> (a,b)
            _       -> ( "static/map.css.tmpl"
                       , "static/data/areas-1b7549470.json" )
  
  -- Read in the data json file
  areas <- readAreas areasfile

  -- Read in the template
  template <- T.readFile templatefile

  -- Process the file 
  for_ (T.lines template) $ \line ->
    if "fmt" `T.isPrefixOf` line
      then do
        let ln = T.splitOn " " line
            fmt = T.unwords $ drop 2 ln
            fil = filterFunction $ T.unwords $ take 1 $ drop 1 ln
        T.putStrLn $ T.intercalate ",\n" ( format areas fmt <$> filter fil areas)
      else
        T.putStrLn line 


filterFunction :: Text -> Area -> Bool
filterFunction "ta" = ("ta"==) . areaLevel
filterFunction "reg" = ("reg"==) . areaLevel
filterFunction "wrd" = ("ward"==) . areaLevel
filterFunction _ = const False

format :: [Area] -> Text -> Area -> Text
format areas =
  let 
    lookupAreaName areaid = maybe "New Zealand" id $ lookup areaid [(areaId a, areaName a) | a <- areas]
    getParentId Area{..} =  ( maybe "nz" id $
              listToMaybe [ p | p <- areaParents 
                              , p /= "new-zealand" ])
    getParentName a = lookupAreaName (getParentId a)
    format' a@Area{..}
       = T.replace "bAreaId" areaId
       . T.replace "bParentId" (getParentId a)
       . T.replace "bAreaName" areaName
       . T.replace "bParentName" (getParentName a)
  in  flip format'



readAreas :: FilePath -> IO [Area]
readAreas file = do
    filecontents <- BS.readFile file
    case eitherDecode filecontents of
        Left err -> error err
        Right vals -> return vals
