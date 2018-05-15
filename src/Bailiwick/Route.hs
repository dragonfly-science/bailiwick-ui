{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.Route
where

-- import Debug.Trace 
import Data.Maybe (listToMaybe, mapMaybe)

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
      segments' = 
          case (segments, message) of
              (["summary", _], SetRegion reg) -> ["summary", reg]
              ([],             SetRegion reg) -> ["summary", reg]
              _                               -> segments
      builder = encodePath segments' []
  in  uri { uriPath = B.toStrict $ toLazyByteString builder }


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
