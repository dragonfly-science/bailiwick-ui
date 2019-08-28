{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase   #-}
module Bailiwick.Route
  ( encodeRoute
  , decodeUri
  , Message(..)
  , Modal(..)
  , getIndicatorId
  , Route(..)
  , ThemePageArgs(..)
  , Page(..)
  , hasAdapter
  , Adapter(..)
  )
where

import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Set (Set, fromList, member)

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Types (encodePath, decodePathSegments)
import URI.ByteString
import qualified Data.Map as M (lookup, fromList)

import Bailiwick.Types

data Message
  = Ready Route
  | SetRegion Text
  | SetSubArea Text
  | SetFeature FeatureId
  | SetAreaType Text
  | SetChartType ChartId
  | SetTransform Text
  | SetYear Year
  | SetYearArea Year Text
  | GoTo Page
  | GoToHomePage
  | ZoomIn
  | ZoomOut (Maybe Text)
  | LeftZoomIn
  | LeftZoomOut (Maybe Text)
  | RightZoomIn
  | RightZoomOut (Maybe Text)
  | SetShowTable
  | UnsetShowTable
  | SetCompareArea Text
  | UnsetCompareArea
  deriving (Eq, Show)

data Route
  = Route
  { routePage        :: Page
  , routeArea        :: AreaId
  , routeCompareArea :: Maybe AreaId
  , routeAdapters    :: Set Adapter
  } deriving (Eq, Show)

data ThemePageArgs
  = ThemePageArgs
  { themePageIndicatorId    :: IndicatorId
  , themePageChartType      :: ChartId
  , themePageYear           :: Year
  , themePageFeatureId      :: Maybe FeatureId
  , themePageDetailId       :: Maybe Text
  , themePageAreaType       :: AreaType
  , themePageTransform      :: TransformId
  } deriving (Eq, Show)

data Page
  = Summary
  | ThemePage ThemePageArgs
  deriving (Eq, Show)
data Adapter
  = Mapzoom
  | LeftZoom
  | RightZoom
  | ShowTable
  deriving (Eq, Show, Ord)

data Modal
  = Download
  | Embed
  | Share
  deriving (Eq, Show)

hasAdapter :: Adapter -> Set Adapter -> Bool
hasAdapter adapter adapters = adapter `member` adapters

getIndicatorId :: Message -> Maybe IndicatorId
getIndicatorId = \case
  GoTo (ThemePage ThemePageArgs{..})
     -> Just themePageIndicatorId
  Ready (Route (ThemePage ThemePageArgs{..}) _ _ _)
     -> Just themePageIndicatorId
  _  -> Nothing



encodeRoute :: Maybe (URIRef Absolute) -> Route -> Text
encodeRoute Nothing _ = ""
encodeRoute (Just uri) route =
  let compareArea mca = [("ca", encodeUtf8 ca) | ca <- maybeToList mca]
      (segments, query) =
        case route of
          Route Summary area mca _ -> (["summary", area], compareArea mca)
          Route (ThemePage (ThemePageArgs (IndicatorId i) (ChartId rc) y f t at lt)) area mca _ ->
                ( ["theme", i, rc, T.pack $ show y, area]
                  <> (featureIdText <$> maybeToList f)
                  <> maybeToList t
                , [("areatype", encodeUtf8 at) | at /= "reg"]
                  <> [("transform", encodeUtf8 lt) | lt /= "absolute"]
                  <> compareArea mca
                )
      cha a = hasAdapter a (routeAdapters route)
      uri' = uri { uriPath = B.toStrict $ toLazyByteString (encodePath segments [])
                 , uriQuery = Query $ query <> [("mapzoom", "1")   | cha Mapzoom]
                                           <> [("rightzoom", "1") | cha RightZoom]
                                           <> [("leftzoom", "1")  | cha LeftZoom]
                                           <> [("showtable", "1") | cha ShowTable]
                 }
  in decodeUtf8 $ serializeURIRef' uri'

decodeUri :: URI -> Route
decodeUri uri =
  let segments = decodePathSegments (uriPath uri)
      Query flags = uriQuery uri
      adapters = fromList $ mapMaybe mkAdapter flags
      mkAdapter ("mapzoom", "1") = Just Mapzoom
      mkAdapter ("rightzoom", "1") = Just RightZoom
      mkAdapter ("leftzoom", "1") = Just LeftZoom
      mkAdapter ("showtable", "1") = Just ShowTable
      mkAdapter _ = Nothing
      homePage = Route Summary "" Nothing adapters
      flagMap = M.fromList flags
      maybeDecodeUtf8 = either (const Nothing) Just . decodeUtf8'
      at = fromMaybe "reg" $ maybeDecodeUtf8 =<< M.lookup "areatype" flagMap
      lt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "transform" flagMap
      ca = maybeDecodeUtf8 =<< M.lookup "ca" flagMap
      standardise = \case
         "wanganui" -> "whanganui"
         good       -> good

  in case segments of
    ["summary", a] -> Route Summary (standardise a) ca adapters
    ("theme":i:rc:y:a:rest) ->
      let fd = case rest of
                     [f, d] -> Just (Just f, Just d)
                     [f] -> Just (Just f, Nothing)
                     [] -> Just (Nothing, Nothing)
                     _ -> Nothing
      in fromMaybe homePage $ (\year (f, d) -> Route (ThemePage (
            ThemePageArgs (IndicatorId i) (ChartId rc) year (FeatureId <$> f) d at lt))
             a ca adapters) <$> readMaybe (T.unpack y) <*> fd
    _  -> homePage

