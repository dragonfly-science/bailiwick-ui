{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick (
    ui
) where

import Control.Monad.Fix

import Reflex.Dom.Core
import Reflex.Dom.Contrib.Router (route')

import qualified Bailiwick.Route as Route
import qualified Bailiwick.Store as Store
import qualified Bailiwick.State as State
import qualified Bailiwick.View  as View

ui :: ( MonadFix m
      , MonadWidget t m
      )  => m ()
ui = mdo
  routeD <- route' Route.encodeUri Route.decodeUri messagesE
  storeD <- Store.run messagesE
  readyE <- getPostBuild
  interactE <- View.view (State.make routeD storeD)
  let messagesE =
         leftmost
            [ let mkMessage r _ = Route.Ready (Route.routePage r)
              in  attachPromptlyDynWith mkMessage routeD readyE
            , traceEvent "interactE: " interactE]
  return ()

