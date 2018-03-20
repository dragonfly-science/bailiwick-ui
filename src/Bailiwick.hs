{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick (
    ui
) where

import Reflex.Dom.Core
import Reflex.Dom.Contrib.Router

import Bailiwick.Route (decodeRoute, encodeRoute)
import Bailiwick.View (view)

ui  :: MonadWidget t m => m ()
ui = mdo
    state <- route' encodeRoute decodeRoute events
    events <- switchDynM (fmap view state)
    return ()


-- Should be in the Reflex.Dom.Core library
switchDynM :: (MonadHold t m, DomBuilder t m, PostBuild t m)
         => Dynamic t (m (Event t a)) -> m (Event t a)
switchDynM = fmap switch . (hold never =<<) . dyn


