{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.Route
where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Types (encodePath, decodePath)
import URI.ByteString

import Bailiwick.State


encodeRoute :: URI -> Message -> URI
encodeRoute uri message = 
  let (segments, query) = decodePath (uriPath uri) 
      segments' = 
          case (segments, message) of
              (["summary", _], SetRegion reg) -> ["summary", reg]
              ([],             SetRegion reg) -> ["summary", reg]
              _ -> segments
      builder = encodePath segments' query
  in  uri { uriPath = B.toStrict $ toLazyByteString builder }


decodeRoute :: URI -> State
decodeRoute uri =
  let (segments, query) = decodePath (uriPath uri) 
  in  case segments of
        ["summary", reg] -> Summary reg
        _                -> Home
