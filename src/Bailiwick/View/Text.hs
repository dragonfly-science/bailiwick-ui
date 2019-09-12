{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.View.Text
  ( textSubstitution
  , capitalize
  )
where


import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Maybe (isJust)
import Data.Char (isSpace, toUpper)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict.InsOrd as M (lookup)

import Bailiwick.Types

capitalize :: Text -> Text
capitalize inp =
  case T.uncons inp of
    Nothing -> ""
    Just (first, rest) -> T.cons (toUpper first) rest


textSubstitution
  :: Loadable Area
  -> Loadable Area
  -> Loadable Indicator
  -> Maybe FeatureId
  -> Maybe DetailId
  -> Maybe Year
  -> Text
  -> Text
textSubstitution area compareArea indicator feature detail year =
    let fy = toMaybe $ indicatorFirstYear <$> indicator
        yem = join $ toMaybe $ indicatorYearEndMonth <$> indicator
        indid = unIndicatorId . indicatorId <$> indicator
        sa = load "New Zealand" areaName area
        aid = load "new-zealand" areaId area
        f = featureIdText <$> feature
        fp = Nothing -- TODO feature types (Tourism spend)
        dl = Nothing -- TODO d <|> (indicatorTopDetailLabel =<< indicator)
        ip = join $ toMaybe $ indicatorPeriod <$> indicator
        p = (-) <$> year <*> ip
        a = case (areaName <$> compareArea) of
              Loaded ca' ->
                     "<span class='active'>" <> sa <>
                     "</span><span class='compare'> (and " <> ca' <> ")</span>"
              _ -> sa
        fl = case indicatorFeatureText <$> indicator of
              Loaded (Just ft) -> (`M.lookup` ft) =<< feature
              _                -> f <|> (join $ toMaybe (indicatorDefaultFeature <$> indicator))
        replace findStr (Just replaceStr) = T.replace findStr replaceStr
        replace _ _ = id
    in T.strip
      . replace "$indid$" (toMaybe indid)
      . replace "$year$" (T.pack . show <$> year)
      . replace "$firstYear$" fy
      . replace "$yearEndMonth$" yem
      . T.replace "$area$" a
      . T.replace "$areaid$" aid
      . T.replace "$selectedArea$" sa
      . replace "$compareArea$" (toMaybe $ areaName <$> compareArea)
      . replace "$compareAreaId$" (toMaybe $ areaId <$> compareArea)
      . replace "$prevYear$" (T.pack . show <$> p)
      . replace "$feature$" fl
      . replace "$featureType$" fp
      . replace "$detail$" dl
      . T.pack
      . removeDetailBrackets detail
      . removeFeatureBrackets (f)
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

