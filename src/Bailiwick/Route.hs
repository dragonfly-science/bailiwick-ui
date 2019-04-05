{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase   #-}
module Bailiwick.Route
  ( encodeUri
  , decodeUri
  , Message(..)
  , Route(..)
  , ThemePageArgs(..)
  , getThemePage
  , Page(..)
  , hasAdapter
  , Adapter(..)
  )
where

import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.List ((\\))

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Types (encodePath, decodePathSegments)
import URI.ByteString
import qualified Data.Map as M (lookup, fromList)

import Bailiwick.Types

data Message
  = Ready
  | SetRegion Text
  | SetSubArea Text
  | SetAreaType Text
  | SetRightChart ChartId
  | SetLeftTransform Text
  | SetRightTransform Text
  | SetYear Year
  | GoTo Page
  | GoToHomePage
  | ZoomIn
  | ZoomOut (Maybe Text)
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
  deriving (Eq, Show)

hasAdapter :: Adapter -> Route -> Bool
hasAdapter adapter Route{..} = adapter `elem` routeAdapters

getThemePage :: Route -> Maybe ThemePageArgs
getThemePage Route{routePage = ThemePage args} = Just args
getThemePage _ = Nothing

updateThemePage :: Route -> (ThemePageArgs -> ThemePageArgs) -> Route
updateThemePage s@Route{routePage = ThemePage args} f = s{ routePage = ThemePage $ f args }
updateThemePage s _ = s


-- import Debug.Trace
step :: Route -> Message -> Route
step route message =
  case message of
    Ready                -> route
    SetRegion reg        -> route { routeArea = reg }
    SetSubArea sa        -> route { routeArea = sa
                                  , routeAdapters = routeAdapters route <> [Mapzoom] }
    SetAreaType at       -> updateThemePage route $ \args -> args { themePageAreaType = at }
    SetRightChart c      -> updateThemePage route $ \args -> args { themePageRightChart = c }
    SetLeftTransform lt  -> updateThemePage route $ \args -> args { themePageLeftTransform = lt }
    SetRightTransform rt -> updateThemePage route $ \args -> args { themePageRightTransform = rt }
    SetYear y            -> updateThemePage route $ \args -> args { themePageYear = y }
    GoTo page            -> route { routePage = page }
    GoToHomePage         -> route { routeArea = "new-zealand"
                                  , routePage = Summary
                                  , routeAdapters = routeAdapters route \\ [Mapzoom] }
    ZoomIn               -> route { routeAdapters = routeAdapters route <> [Mapzoom] }
    ZoomOut (Just reg)   -> route { routeArea = reg
                                  , routeAdapters = routeAdapters route \\ [Mapzoom] }
    ZoomOut Nothing      -> route { routeAdapters = routeAdapters route \\ [Mapzoom] }

encodeRoute :: Route -> URI -> URI
encodeRoute route uri =
  let (segments, query) =
        case route of
          Route Summary area _ _ -> (["summary", area], [])
          Route (ThemePage (ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) y f t at lt rt)) area _ _ ->
                ( ["theme", i, lc, rc, T.pack $ show y, area] <> (featureIdText <$> maybeToList f) <> maybeToList t
                , [("areatype", encodeUtf8 at) | at /= "reg"] <> [("left-transform", encodeUtf8 lt) | lt /= "absolute"] <> [("right-transform", encodeUtf8 rt)]
                )
  in uri { uriPath = B.toStrict $ toLazyByteString (encodePath segments [])
         , uriQuery = Query $ query <> [("mapzoom", "1") | hasAdapter Mapzoom route]
         }

encodeUri :: URI -> Message -> URI
encodeUri uri message = encodeRoute (step (decodeUri uri) message) uri

decodeUri :: URI -> Route
decodeUri uri =
  let segments = decodePathSegments (uriPath uri)
      Query flags = uriQuery uri
      adapters = mapMaybe mkAdapter flags
      mkAdapter ("mapzoom", "1") = Just Mapzoom
      mkAdapter _ = Nothing
      homePage = Route Summary "" Nothing adapters
      flagMap = M.fromList flags
      maybeDecodeUtf8 = either (const Nothing) Just . decodeUtf8'
      at = fromMaybe "reg" $ maybeDecodeUtf8 =<< M.lookup "areatype" flagMap
      lt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "left-transform" flagMap
      rt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "right-transform" flagMap
      standardise = \case
         "wanganui" -> "whanganui"
         good       -> good

  in case segments of
    ["summary", a] -> Route Summary (standardise a) Nothing adapters
    ("theme":i:lc:rc:y:a:rest) ->
      let fd = case rest of
                     [f, d] -> Just (Just f, Just d)
                     [f] -> Just (Just f, Nothing)
                     [] -> Just (Nothing, Nothing)
                     _ -> Nothing
      in fromMaybe homePage $ (\year (f, d) -> Route (ThemePage (
            ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) year (FeatureId <$> f) d at lt rt)) a Nothing adapters) <$> readMaybe (T.unpack y) <*> fd
    _  -> homePage

