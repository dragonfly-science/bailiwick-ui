{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.Route
where

-- import Debug.Trace
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List ((\\))

import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Types (encodePath, decodePathSegments)
import URI.ByteString
import qualified Data.Map.Ordered as OMap

import Bailiwick.State
import Bailiwick.Types


encodeRoute :: URI -> Message -> URI
encodeRoute uri message =
  let segments = decodePathSegments (uriPath uri)
      Query flags = uriQuery uri
      segments' =
          case (segments, message) of
              (["summary", _], SetRegion reg)      -> ["summary", reg]
              (["summary", _], ZoomOut (Just reg)) -> ["summary", reg]
              (["summary", _], SetSubArea sa)      -> ["summary", sa]
              ([],             SetRegion reg)      -> ["summary", reg]
              ([],             ZoomOut (Just reg)) -> ["summary", reg]
              ([],             SetSubArea sa)      -> ["summary", sa]
              _                                    -> segments
      builder = encodePath segments' []
      flags' = updateFlag message flags
  in  uri { uriPath = B.toStrict $ toLazyByteString builder
          , uriQuery = Query flags'
          }
  where
    updateFlag ZoomIn flags
      | ("mapzoom", "1") `notElem` flags =
                                   flags ++ [("mapzoom", "1")]
    updateFlag (ZoomOut _) flags = flags \\ [("mapzoom", "1")]
    updateFlag _ flags = flags


decodeRoute :: Areas -> URI -> State
decodeRoute areas uri =
  let segments = decodePathSegments (uriPath uri)
      Query flags = uriQuery uri
      adapters = mapMaybe mkAdapter flags
      mkAdapter ("mapzoom", "1") = Just Mapzoom
      mkAdapter _ = Nothing

      path = case segments of
                ["summary", a] -> a
                _              -> "new-zealand"
      area = OMap.lookup path areas
      parent = do
        a <- area
        -- TODO: handle accessedvia
        listToMaybe [ parentArea
                    | parentArea <- mapMaybe (flip OMap.lookup areas) (areaParents a)
                    , areaLevel parentArea == "reg" ]

      page = if path == "new-zealand"
                then Home
                else case (area, parent) of
                        (Just a, Just b)  -> Summary [b, a]
                        (Just a, Nothing) -> Summary [a]
                        _                 -> Home
  in  State page adapters
