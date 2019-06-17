{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.View.Text
  ( textSubstitution
  , capitalize
  )
where


import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import Data.Char (isSpace, toUpper)


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict.InsOrd as M (lookup)

import Bailiwick.Route
import Bailiwick.Types


capitalize :: Text -> Text
capitalize inp =
  case T.uncons inp of
    Nothing -> ""
    Just (first, rest) -> T.cons (toUpper first) rest


textSubstitution
  :: Maybe Area
  -> Maybe Area
  -> Maybe Indicator
  -> Maybe Feature
  -> Maybe ThemePageArgs
  -> Text
  -> Text
textSubstitution area compareArea indicator feature themePage =
    let y = themePageYear <$> themePage
        fy = indicatorFirstYear <$> indicator
        yem = indicatorYearEndMonth =<< indicator
        sa = maybe "New Zealand" areaName area
        f = featureName <$> feature
        fp = if isJust f then featureParent =<< feature else Just ""
        _d = themePageDetailId <$> themePage --
        dl = Nothing -- TODO d <|> (indicatorTopDetailLabel =<< indicator)
        ip = indicatorPeriod =<< indicator
        p = (-) <$> y <*> ip
        a = case (areaName <$> compareArea) of
              Just ca' ->
                     "<span class='active'>" <> sa <>
                     "</span><span class='compare'> (and " <> ca' <> ")</span>"
              _ -> sa
        fl = case indicatorFeatureText =<< indicator of
              Just ft -> (`M.lookup` ft) =<< themePageFeatureId =<< themePage
              _ -> f <|> (indicatorDefaultFeature =<< indicator)
        replace findStr (Just replaceStr) = T.replace findStr replaceStr
        replace _ _ = id
    in T.strip
      . replace "$year$" (T.pack . show <$> y)
      . replace "$firstYear$" fy
      . replace "$yearEndMonth$" yem
      . T.replace "$area$" a
      . T.replace "$selectedArea$" sa
      . replace "$compareArea$" (areaName <$> compareArea)
      . replace "$prevYear$" (T.pack . show <$> p)
      . replace "$feature$" fl
      . replace "$featureType$" fp
      . replace "$detail$" dl
      . T.pack
      . removeDetailBrackets (themePageDetailId =<< themePage)
      . removeFeatureBrackets (featureName <$> feature)
      . T.unpack


removeFeatureBrackets :: Maybe Text -> String -> String
removeFeatureBrackets feature =
  if isJust feature
    then filter (\c -> c /= '[' && c /= ']')
    else go
  where
    go :: String -> String
    go "" = ""
    go ('[':xs) = go . drop 1 $ dropWhile (/= ']') xs
    go (x:xs) = x:go xs

removeDetailBrackets :: Maybe Text -> String -> String
removeDetailBrackets detail =
  if isJust detail
    then filter (\c -> c /= '{' && c /= '}')
    else go
  where
    go :: String -> String
    go "" = ""
    go ('{':xs) = go . drop 1 $ dropWhile (/= '}') xs
    go (s:'{':xs) | isSpace s = go . drop 1 $ dropWhile (/= '}') xs
    go (x:xs) = x:go xs

