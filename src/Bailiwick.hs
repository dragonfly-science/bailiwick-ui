{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick (
    ui
) where

import Reflex.Dom.Core
import Reflex.Dom.Contrib.Router

import Bailiwick.Route (decodeRoute, encodeRoute)
import Bailiwick.State (Message)
import Bailiwick.View (view)

ui  :: MonadWidget t m => m ()
ui = do
  rec
      state <- route' encodeRoute decodeRoute events
      events <- view state
  return ()



