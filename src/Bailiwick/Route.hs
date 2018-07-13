{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.Route
where

-- import Debug.Trace
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe, maybeToList)
import Data.List ((\\))
import qualified Data.Text as T (pack, unpack)
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Types (encodePath, decodePathSegments)
import URI.ByteString
import qualified Data.HashMap.Strict.InsOrd as OMap

import Bailiwick.State
import Bailiwick.Types
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Map as M (lookup, fromList)

step :: Areas -> State -> Message -> State
step areas state message =
  case message of
    SetRegion reg        -> state { stateArea = areaList areas reg }
    SetSubArea sa        -> state { stateArea = areaList areas sa }
    SetAreaType at       -> updateThemePage state $ \args -> args { themePageAreaType = at }
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
          State Summary [] _ -> ([], [])
          State Summary areas _ -> (["summary", areaId $ last areas], [])
          State (ThemePage (ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) y f t at lt rt)) areas _ ->
                ( ["theme", i, lc, rc, T.pack $ show y, last ("new-zealand":map areaId areas)] <> maybeToList f <> maybeToList t
                , [("areatype", encodeUtf8 at) | at /= "reg"] <> [("left-transform", encodeUtf8 lt) | lt /= "absolute"] <> [("right-transform", encodeUtf8 rt)]
                )
  in uri { uriPath = B.toStrict $ toLazyByteString (encodePath segments [])
         , uriQuery = Query $ query <> [("mapzoom", "1") | hasAdapter Mapzoom state]
         }

encodeRoute :: Areas -> URI -> Message -> URI
encodeRoute areas uri message = encodeState (step areas (decodeRoute areas uri) message) uri

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

decodeRoute :: Areas -> URI -> State
decodeRoute areas uri =
  let segments = decodePathSegments (uriPath uri)
      Query flags = uriQuery uri
      adapters = mapMaybe mkAdapter flags
      mkAdapter ("mapzoom", "1") = Just Mapzoom
      mkAdapter _ = Nothing
      homePage = State Summary [] adapters
      flagMap = M.fromList flags
      maybeDecodeUtf8 = either (const Nothing) Just . decodeUtf8'
      at = fromMaybe "reg" $ maybeDecodeUtf8 =<< M.lookup "areatype" flagMap
      lt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "left-transform" flagMap
      rt = fromMaybe "absolute" $ maybeDecodeUtf8 =<< M.lookup "right-transform" flagMap

  in case segments of
    ["summary", a] -> State Summary (areaList areas a) adapters
    ("theme":i:lc:rc:y:a:rest) ->
      let fd = case rest of
                     [f, d] -> Just (Just f, Just d)
                     [f] -> Just (Just f, Nothing)
                     [] -> Just (Nothing, Nothing)
                     _ -> Nothing
      in fromMaybe homePage $ (\year (f, d) -> State (ThemePage (
            ThemePageArgs (IndicatorId i) (ChartId lc) (ChartId rc) year f d at lt rt)) (areaList areas a) adapters) <$> readMaybe (T.unpack y) <*> fd
    _  -> homePage
