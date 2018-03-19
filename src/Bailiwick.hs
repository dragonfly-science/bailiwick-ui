{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick (
    ui
) where

import Reflex.Dom.Core
import Reflex.Dom.Routing.Nested (runRouteWithPathInFragment)
import Reflex.Dom.Routing.Writer (runRouteWriterT)

ui  :: MonadWidget t m => m ()
ui =
  runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ mdo

    el "h1" (text "hello")

    return ()


