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
import Reflex.Dom.Contrib.Router

import Language.Javascript.JSaddle.Types (MonadJSM)

import qualified Bailiwick.Store as Store
import Bailiwick.Route (decodeRoute, encodeRoute)
import Bailiwick.View (view)

ui  :: ( Monad m
       , MonadJSM m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , DomBuilderSpace m ~ GhcjsDomSpace
       , MonadWidget t m
      )  => m ()
ui = do
  ready <- getPostBuild
  storeD <- Store.initialise ready

  dyn_ $ do
    areas <- Store.getAreas <$> storeD
    return $ mdo
      stateD <- route' (encodeRoute areas) (decodeRoute areas) eventsE
      eventsE <- view storeD stateD
      return ()

