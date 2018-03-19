module Bailiwick.Route
where

import URI.ByteString

import Bailiwick.State


encodeRoute :: URI -> Message -> URI
encodeRoute uri (SetPath path) = uri { uriPath = path }


decodeRoute :: URI -> State
decodeRoute uri = State (uriPath uri)
