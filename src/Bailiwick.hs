{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick (
    ui
) where

import Control.Monad (void)
import Control.Monad.Fix

import Reflex.Dom.Core
import Reflex.Dom.Contrib.Router (route, getURI)

import qualified Bailiwick.Route as Route
import qualified Bailiwick.State as State
import qualified Bailiwick.View  as View

ui :: ( MonadFix m
      , MonadWidget t m
      )  => m ()
ui = do
  readyE <- getPostBuild
  initialURIE <- performEvent (getURI <$ readyE)
  let initialRouteE = Route.Ready . Route.decodeUri <$> initialURIE

  rec
    void $ route never -- (Route.encodeRoute <$> routeE)

    state <- State.run (traceEvent "messagesE" $ leftmost [initialRouteE, messageE])
    -- routeE <- State.route state

    messageE <- View.view state

  return ()

