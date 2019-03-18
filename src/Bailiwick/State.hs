{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.State
  ( runState
  , Message(..)
  , State(..)
  , Page(..)
  , ThemePageArgs(..)
  , getArea
  , getRegion
  , getSubArea
  , getPage
  , getThemePage
  , hasAdapter
  , Adapter(..)
  )
where

import Data.Maybe (listToMaybe, mapMaybe, fromMaybe, maybeToList)
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
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Map as M (lookup, fromList)

import Reflex.Dom.Contrib.Router (route')
import Reflex.Dom.Core

import Bailiwick.Types


-- The application has two parts of the state:
--   1. The selected indicator, area, year, feature etc.,
--   2. The selected presentation.
-- The state is held in a single dynamic at the top level.

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
  , stateArea        :: [Area]
  , stateCompareArea :: Maybe Area
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


getRegion :: State -> Maybe Area
getRegion (State _ (reg:_) _ _) = Just reg
getRegion _ = Nothing

getSubArea :: State -> Maybe Area
getSubArea (State _ (_:subarea:_) _ _) = Just subarea
getSubArea _ = Nothing

getArea :: State -> Maybe Area
getArea = listToMaybe . reverse . stateArea

getCompareArea :: State -> Maybe Area
getCompareArea (State _ [] _ _)    = Nothing
getCompareArea (State _ areas _ _) = Just $ last areas

getThemePage :: State -> Maybe ThemePageArgs
getThemePage State{statePage = ThemePage args} = Just args
getThemePage _ = Nothing

updateThemePage :: State -> (ThemePageArgs -> ThemePageArgs) -> State
updateThemePage s@State{statePage = ThemePage args} f = s{ statePage = ThemePage $ f args }
updateThemePage s _ = s


-- import Debug.Trace
step :: Areas -> State -> Message -> State
step areas state message =
  case message of
    SetRegion reg        -> state { stateArea = areaList areas reg }
    SetSubArea sa        -> state { stateArea = areaList areas sa }
    SetAreaType at       -> updateThemePage state $ \args -> args { themePageAreaType = at }
    SetRightChart c      -> updateThemePage state $ \args -> args { themePageRightChart = c }
    SetLeftTransform lt  -> updateThemePage state $ \args -> args { themePageLeftTransform = lt }
    SetRightTransform rt -> updateThemePage state $ \args -> args { themePageRightTransform = rt }
    SetYear y            -> updateThemePage state $ \args -> args { themePageYear = y }
    GoTo page            -> state { statePage = page }
    GoToHomePage         -> state { stateArea = [], statePage = Summary }
    ZoomIn               -> state { stateAdapters = stateAdapters state <> [Mapzoom] }
    ZoomOut (Just reg)   -> state { stateArea = areaList areas reg
                                  , stateAdapters = stateAdapters state \\ [Mapzoom] }
    ZoomOut Nothing      -> state { stateAdapters = stateAdapters state \\ [Mapzoom] }

encodeState :: State -> URI -> URI
encodeState state uri =
  let (segments, query) =
        case state of
          State Summary [] _ _ -> ([], [])
          State Summary areas _ _ -> (["summary", areaId $ last areas], [])
          State (ThemePage (ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) y f t at lt rt)) areas _ _ ->
                ( ["theme", i, lc, rc, T.pack $ show y, last ("new-zealand":map areaId areas)] <> (featureIdText <$> maybeToList f) <> maybeToList t
                , [("areatype", encodeUtf8 at) | at /= "reg"] <> [("left-transform", encodeUtf8 lt) | lt /= "absolute"] <> [("right-transform", encodeUtf8 rt)]
                )
  in uri { uriPath = B.toStrict $ toLazyByteString (encodePath segments [])
         , uriQuery = Query $ query <> [("mapzoom", "1") | hasAdapter Mapzoom state]
         }

encodeRoute :: URI -> Message -> URI
encodeRoute = undefined
--encodeRoute areas uri message = encodeState (step areas (decodeRoute areas uri) message) uri

areaList :: Areas -> Text -> [Area]
areaList _ "new-zealand" = []
areaList areas p = case (area, parent) of
                  (Just a, Just b)  -> [b, a]
                  (Just a, Nothing) -> [a]
                  _                 -> []
  where
    area = OMap.lookup p areas
    parent = do
      a <- area
      -- TODO: handle accessedvia
      listToMaybe [ parentArea
                  | parentArea <- mapMaybe (`OMap.lookup` areas) (areaParents a)
                  , areaLevel parentArea == "reg" ]

decodeRoute :: URI -> State
decodeRoute = undefined
--decodeRoute areas uri =
--  let segments = decodePathSegments (uriPath uri)
--      Query flags = uriQuery uri
--      adapters = mapMaybe mkAdapter flags
--      mkAdapter ("mapzoom", "1") = Just Mapzoom
--      mkAdapter _ = Nothing
--      homePage = State Summary [] Nothing adapters
--      flagMap = M.fromList flags
--      maybeDecodeUtf8 = either (const Nothing) Just . decodeUtf8'
--      at = fromMaybe "reg" $ maybeDecodeUtf8 =<< M.lookup "areatype" flagMap
--      lt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "left-transform" flagMap
--      rt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "right-transform" flagMap
--
--  in case segments of
--    ["summary", a] -> State Summary (areaList areas a) Nothing adapters
--    ("theme":i:lc:rc:y:a:rest) ->
--      let fd = case rest of
--                     [f, d] -> Just (Just f, Just d)
--                     [f] -> Just (Just f, Nothing)
--                     [] -> Just (Nothing, Nothing)
--                     _ -> Nothing
--      in fromMaybe homePage $ (\year (f, d) -> State (ThemePage (
--            ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) year (FeatureId <$> f) d at lt rt)) (areaList areas a) Nothing adapters) <$> readMaybe (T.unpack y) <*> fd
--    _  -> homePage
