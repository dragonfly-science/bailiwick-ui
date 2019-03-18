{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.State
  ( runState
  , Message(..)
  , State(..)
  , Page(..)
  , ThemePageArgs(..)
  , getPage
  , getThemePage
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

import Reflex.Dom.Contrib.Router (route')
import Reflex.Dom.Core

import Bailiwick.Types

runState
  :: ( MonadWidget t m
     )
  => Event t Message -> m (Dynamic t State)
runState = route' encodeRoute decodeRoute

data Message
  = SetRegion Text
  | SetSubArea Text
  | SetAreaType Text
  | SetRightChart ChartId
  | SetLeftTransform Text
  | SetRightTransform Text
  | SetYear Int
  | GoTo Page
  | GoToHomePage
  | ZoomIn
  | ZoomOut (Maybe Text)
  deriving (Eq, Show)

data State
  = State
  { statePage        :: Page
  , stateArea        :: Text
  , stateCompareArea :: Maybe Text
  , stateAdapters    :: [Adapter]
  } deriving (Eq, Show)

data ThemePageArgs
  = ThemePageArgs
  { themePageIndicatorId    :: IndicatorId
  , themePageLeftChart      :: ChartId
  , themePageRightChart     :: ChartId
  , themePageYear           :: Int
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

getPage :: State -> Page
getPage (State page _ _ _) = page

hasAdapter :: Adapter -> State -> Bool
hasAdapter adapter State{..} = adapter `elem` stateAdapters

getThemePage :: State -> Maybe ThemePageArgs
getThemePage State{statePage = ThemePage args} = Just args
getThemePage _ = Nothing

updateThemePage :: State -> (ThemePageArgs -> ThemePageArgs) -> State
updateThemePage s@State{statePage = ThemePage args} f = s{ statePage = ThemePage $ f args }
updateThemePage s _ = s


-- import Debug.Trace
step :: State -> Message -> State
step state message =
  case message of
    SetRegion reg        -> state { stateArea = reg }
    SetSubArea sa        -> state { stateArea = sa }
    SetAreaType at       -> updateThemePage state $ \args -> args { themePageAreaType = at }
    SetRightChart c      -> updateThemePage state $ \args -> args { themePageRightChart = c }
    SetLeftTransform lt  -> updateThemePage state $ \args -> args { themePageLeftTransform = lt }
    SetRightTransform rt -> updateThemePage state $ \args -> args { themePageRightTransform = rt }
    SetYear y            -> updateThemePage state $ \args -> args { themePageYear = y }
    GoTo page            -> state { statePage = page }
    GoToHomePage         -> state { stateArea = "new-zealand", statePage = Summary }
    ZoomIn               -> state { stateAdapters = stateAdapters state <> [Mapzoom] }
    ZoomOut (Just reg)   -> state { stateArea = reg
                                  , stateAdapters = stateAdapters state \\ [Mapzoom] }
    ZoomOut Nothing      -> state { stateAdapters = stateAdapters state \\ [Mapzoom] }

encodeState :: State -> URI -> URI
encodeState state uri =
  let (segments, query) =
        case state of
          State Summary area _ _ -> (["summary", area], [])
          State (ThemePage (ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) y f t at lt rt)) area _ _ ->
                ( ["theme", i, lc, rc, T.pack $ show y, area] <> (featureIdText <$> maybeToList f) <> maybeToList t
                , [("areatype", encodeUtf8 at) | at /= "reg"] <> [("left-transform", encodeUtf8 lt) | lt /= "absolute"] <> [("right-transform", encodeUtf8 rt)]
                )
  in uri { uriPath = B.toStrict $ toLazyByteString (encodePath segments [])
         , uriQuery = Query $ query <> [("mapzoom", "1") | hasAdapter Mapzoom state]
         }

encodeRoute :: URI -> Message -> URI
encodeRoute uri message = encodeState (step (decodeRoute uri) message) uri

decodeRoute :: URI -> State
decodeRoute uri =
  let segments = decodePathSegments (uriPath uri)
      Query flags = uriQuery uri
      adapters = mapMaybe mkAdapter flags
      mkAdapter ("mapzoom", "1") = Just Mapzoom
      mkAdapter _ = Nothing
      homePage = State Summary "" Nothing adapters
      flagMap = M.fromList flags
      maybeDecodeUtf8 = either (const Nothing) Just . decodeUtf8'
      at = fromMaybe "reg" $ maybeDecodeUtf8 =<< M.lookup "areatype" flagMap
      lt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "left-transform" flagMap
      rt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "right-transform" flagMap

  in case segments of
    ["summary", a] -> State Summary a Nothing adapters
    ("theme":i:lc:rc:y:a:rest) ->
      let fd = case rest of
                     [f, d] -> Just (Just f, Just d)
                     [f] -> Just (Just f, Nothing)
                     [] -> Just (Nothing, Nothing)
                     _ -> Nothing
      in fromMaybe homePage $ (\year (f, d) -> State (ThemePage (
            ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) year (FeatureId <$> f) d at lt rt)) a Nothing adapters) <$> readMaybe (T.unpack y) <*> fd
    _  -> homePage
