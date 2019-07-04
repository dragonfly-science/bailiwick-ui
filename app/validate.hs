{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where


import System.Exit
import System.Environment (getArgs, getProgName)
import System.FilePath
import Control.Monad (forM_)

import qualified Data.Text as Text
import Data.Aeson (FromJSON, eitherDecodeFileStrict')

import Bailiwick.Types


main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> validate dir
    _ -> do
      progname <- getProgName
      putStrLn $ "Usage: " ++ progname ++ " <jsondir> "


validate :: FilePath -> IO ()
validate dir = do
  _areas :: Areas <-  validateFile $ dir </> "areas.json"
  _area_summaries :: AreaSummaries <- validateFile $ dir </> "area-summaries.json"
  themes :: [Theme] <-  validateFile $ dir </> "themes.json"
  forM_ themes $ \Theme{..} -> do
    putStrLn $ "Theme: " ++ (Text.unpack themeName)
    forM_ themeIndicators $ \Indicator{..} -> do
      _id :: IndicatorData  <- validateFile $ dir </> (Text.unpack (unIndicatorId indicatorId)) <.> "json"
      return ()

  putStrLn "Done"
  exitSuccess



validateFile :: FromJSON a => FilePath -> IO a
validateFile file = do
  result <- eitherDecodeFileStrict' file
  case result of
    Left err -> do
      putStrLn $ "    parse error for file " ++ file ++ ": " ++ err
      exitFailure
    Right res -> do
      putStrLn $ "File " ++ file ++ " parsed okay"
      return res

