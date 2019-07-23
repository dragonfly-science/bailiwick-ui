{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase   #-}
module Bailiwick.Route
  ( encodeUri
  , decodeUri
  , Message(..)
  , getIndicatorId
  , Route(..)
  , ThemePageArgs(..)
  , getThemePage
  , Page(..)
  , hasAdapter
  , Adapter(..)
  )
where

import Data.Maybe (mapMaybe, fromMaybe, maybeToList, isNothing)
import Data.Monoid ((<>))
import Data.List ((\\))

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, isPrefixOf)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Types (encodePath, decodePathSegments)
import URI.ByteString
import qualified Data.Map as M (lookup, fromList)

import Bailiwick.Types

data Message
  = Ready Page
  | SetRegion Text
  | SetSubArea Text
  | SetFeature FeatureId
  | SetAreaType Text
  | SetRightChart ChartId
  | SetLeftTransform Text
  | SetRightTransform Text
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
  , routeArea        :: Text
  , routeCompareArea :: Maybe Text
  , routeAdapters    :: [Adapter]
  } deriving (Eq, Show)

data ThemePageArgs
  = ThemePageArgs
  { themePageIndicatorId    :: IndicatorId
  , themePageLeftChart      :: ChartId
  , themePageRightChart     :: ChartId
  , themePageYear           :: Year
  , themePageFeatureId      :: Maybe FeatureId
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
  | LeftZoom
  | RightZoom
  | ShowTable
  deriving (Eq, Show)

hasAdapter :: Adapter -> Route -> Bool
hasAdapter adapter Route{..} = adapter `elem` routeAdapters

getThemePage :: Route -> Maybe ThemePageArgs
getThemePage Route{routePage = ThemePage args} = Just args
getThemePage _ = Nothing

isSummary :: Route -> Bool
isSummary = isNothing . getThemePage

updateTP :: (ThemePageArgs -> ThemePageArgs) -> Route -> Route
updateTP f s@Route{routePage = ThemePage args}
    = s{ routePage = ThemePage $ f args }
updateTP _ s = s

getIndicatorId :: Message -> Maybe IndicatorId
getIndicatorId = \case
  GoTo (ThemePage ThemePageArgs{..})
     -> Just themePageIndicatorId
  Ready (ThemePage ThemePageArgs{..})
     -> Just themePageIndicatorId
  _  -> Nothing


-- import Debug.Trace
step :: Route -> Message -> Route
step route message =
  case message of

    Ready _page
        -> route

    SetRegion reg
        -> let route' = route { routeArea = reg }
               update args = args { themePageAreaType = "reg" }
           in  updateTP update route'

    SetSubArea sa
        -> let route' = route { routeArea = sa
                              , routeAdapters = routeAdapters route <> [Mapzoom, LeftZoom, RightZoom]
                              }
               at = if T.isPrefixOf "auckland" sa
                      then "ward"
                      else "ta"
               update args = args { themePageAreaType = at }
           in  updateTP update route'

    SetFeature feature
        -> let update args = args { themePageFeatureId = Just feature }
           in  updateTP update route


    SetAreaType at
        -> let update args = args { themePageAreaType = at }
           in  updateTP update route

    SetRightChart c
        -> let update args = args { themePageRightChart = c }
           in  updateTP update route

    SetLeftTransform lt
        -> let update args = args { themePageLeftTransform = lt }
           in  updateTP update route

    SetRightTransform rt
        -> let update args = args { themePageRightTransform = rt }
           in  updateTP update route

    SetYear y
        -> let update args = args { themePageYear = y }
           in  updateTP update route

    SetYearArea y a
        -> let route' = route { routeArea = a }
               update args = args { themePageYear = y }
           in  updateTP update route'

    GoTo page
        -> route { routePage = page }

    GoToHomePage
        -> route { routeArea = "new-zealand"
                 , routePage = Summary
                 , routeAdapters = routeAdapters route \\ [Mapzoom, LeftZoom, RightZoom]
                 }

    ZoomIn
        -> route { routeAdapters = routeAdapters route <> [Mapzoom] }

    LeftZoomIn
        -> route { routeAdapters = routeAdapters route <> [LeftZoom] }

    RightZoomIn
        -> route { routeAdapters = routeAdapters route <> [RightZoom] }

    ZoomOut (Just reg)
        -> if isSummary route
             then route { routeArea = reg
                        , routeAdapters = routeAdapters route \\ [Mapzoom]
                        }
             else route { routeAdapters = routeAdapters route \\ [Mapzoom] }

    ZoomOut Nothing
        -> route { routeAdapters = routeAdapters route \\ [LeftZoom] }

    LeftZoomOut (Just reg)
        -> if isSummary route
             then route { routeArea = reg
                        , routeAdapters = routeAdapters route \\ [LeftZoom]
                        }
             else route { routeAdapters = routeAdapters route \\ [LeftZoom] }

    LeftZoomOut Nothing
        -> route { routeAdapters = routeAdapters route \\ [LeftZoom] }

    RightZoomOut (Just reg)
        -> if isSummary route
             then route { routeArea = reg
                        , routeAdapters = routeAdapters route \\ [RightZoom]
                        }
             else route { routeAdapters = routeAdapters route \\ [RightZoom] }

    RightZoomOut Nothing
        -> route { routeAdapters = routeAdapters route \\ [RightZoom] }

    SetShowTable
        -> route { routeAdapters = routeAdapters route <> [ShowTable] }

    UnsetShowTable
        -> route { routeAdapters = routeAdapters route \\ [ShowTable] }

    SetCompareArea area
        -> route { routeCompareArea = Just area }

    UnsetCompareArea
        -> route { routeCompareArea = Nothing }


encodeRoute :: Route -> URI -> URI
encodeRoute route uri =
  let compareArea mca = [("ca", encodeUtf8 ca) | ca <- maybeToList mca]
      (segments, query) =
        case route of
          Route Summary area mca _ -> (["summary", area], compareArea mca)
          Route (ThemePage (ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) y f t at lt rt)) area mca _ ->
                ( ["theme", i, lc, rc, T.pack $ show y, area]
                  <> (featureIdText <$> maybeToList f)
                  <> maybeToList t
                , [("areatype", encodeUtf8 at) | at /= "reg"]
                  <> [("left-transform", encodeUtf8 lt) | lt /= "absolute"]
                  <> [("right-transform", encodeUtf8 rt)]
                  <> compareArea mca
                )
  in uri { uriPath = B.toStrict $ toLazyByteString (encodePath segments [])
         , uriQuery = Query $ query <> [("mapzoom", "1")   | hasAdapter Mapzoom route]
                                    <> [("rightzoom", "1") | hasAdapter RightZoom route]
                                    <> [("leftzoom", "1")  | hasAdapter LeftZoom route]
                                    <> [("showtable", "1") | hasAdapter ShowTable route]
         }

encodeUri :: URI -> Message -> URI
encodeUri uri message = encodeRoute (step (decodeUri uri) message) uri

decodeUri :: URI -> Route
decodeUri uri =
  let segments = decodePathSegments (uriPath uri)
      Query flags = uriQuery uri
      adapters = mapMaybe mkAdapter flags
      mkAdapter ("mapzoom", "1") = Just Mapzoom
      mkAdapter ("rightzoom", "1") = Just RightZoom
      mkAdapter ("leftzoom", "1") = Just LeftZoom
      mkAdapter ("showtable", "1") = Just ShowTable
      mkAdapter _ = Nothing
      homePage = Route Summary "" Nothing adapters
      flagMap = M.fromList flags
      maybeDecodeUtf8 = either (const Nothing) Just . decodeUtf8'
      at = fromMaybe "reg" $ maybeDecodeUtf8 =<< M.lookup "areatype" flagMap
      lt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "left-transform" flagMap
      rt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "right-transform" flagMap
      ca = maybeDecodeUtf8 =<< M.lookup "ca" flagMap
      standardise = \case
         "wanganui" -> "whanganui"
         good       -> good

  in case segments of
    ["summary", a] -> Route Summary (standardise a) ca adapters
    ("theme":i:lc:rc:y:a:rest) ->
      let fd = case rest of
                     [f, d] -> Just (Just f, Just d)
                     [f] -> Just (Just f, Nothing)
                     [] -> Just (Nothing, Nothing)
                     _ -> Nothing
      in fromMaybe homePage $ (\year (f, d) -> Route (ThemePage (
            ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) year (FeatureId <$> f) d at lt rt))
             a ca adapters) <$> readMaybe (T.unpack y) <*> fd
    _  -> homePage

