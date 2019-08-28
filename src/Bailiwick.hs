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
import Reflex.Dom.Contrib.Router (route)

import qualified Bailiwick.Route as Route
import qualified Bailiwick.State as State
import qualified Bailiwick.View  as View

ui :: ( MonadFix m
      , MonadWidget t m
      )  => m ()
ui = do
  readyE <- getPostBuild

  rec
    uriD <- route $ attachPromptlyDynWith Route.encodeRoute
                                          initialUriD
                                          (State.route state)
    let uriE = tag (current uriD) readyE
    initialUriD <- holdDyn Nothing (Just <$> uriE)

    state <- State.run $ leftmost [ Route.Ready . Route.decodeUri <$> uriE
                                  , messageE
                                  ]

    messageE <- View.view state

  return ()

