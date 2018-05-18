{-# LANGUAGE ScopedTypeVariables #-}
module Main where
{- Script to make the map.css file, based on the area data -}

import System.Environment (getArgs)
import Data.Monoid ((<>))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS

import Bailiwick.Types


main :: IO ()
main = do
  
  args <- getArgs
  let jsonfile
        = case args of
            (a:_) -> a
            _     -> "static/data/areas-1b7549470.json"
  
  -- Read in the data json file
  areas :: [Area] <- readAreas jsonfile

  -- Produce a list of CSS selectors for ta's when zoomed
  let selectors :: [ BS.ByteString ]
      selectors = [
  div.canterbury.canterbury-zoom.map g.area[reg="Canterbury"] > path



readAreas :: FilePath -> IO [Area]
readAreas file = do
    filecontents <- BS.readFile file
    case eitherDecode filecontents of
        Left err -> error err
        Right vals -> return vals
