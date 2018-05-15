{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.Route
where

import Data.Maybe (listToMaybe)

import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Types (encodePath, decodePath)
import URI.ByteString
import qualified Data.Map.Ordered as OMap

import Bailiwick.State
import Bailiwick.Types


encodeRoute :: URI -> Message -> URI
encodeRoute uri message = 
  let (segments, query) = decodePath (uriPath uri) 
      segments' = 
          case (segments, message) of
              (["summary", _], SetRegion reg) -> ["summary", reg]
              ([],             SetRegion reg) -> ["summary", reg]
              _                               -> segments
      builder = encodePath segments' query
  in  uri { uriPath = B.toStrict $ toLazyByteString builder }


decodeRoute :: Areas -> URI -> State
decodeRoute areas uri =
  let (segments, query) = decodePath (uriPath uri) 
      path = case segments of
                ["summary", a] -> a
                _              -> "new-zealand"
      area = OMap.lookup path areas
      parent = do
        a <- area  
        p <- listToMaybe $ areaParents a  -- TODO: handle accessedvia
        parentArea <- OMap.lookup p areas
        if areaLevel parentArea == "reg"
            then Just parentArea
            else Nothing
      page = case (area, parent) of
                (Just a, Just b)  -> Summary [a,b]
                (Just a, Nothing) -> Summary [a]
                _                 -> Home
  in if path == "new-zealand" 
        then State Home Adapters
        else State page Adapters
