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
import Servant.Reflex

import Bailiwick.Store (getAreas)
import Bailiwick.Route (decodeRoute, encodeRoute)
import Bailiwick.View (view)
import Bailiwick.Types

ui  :: ( Monad m
       , MonadJSM m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , SupportsServantReflex t m
       , DomBuilderSpace m ~ GhcjsDomSpace
       , MonadWidget t m
      )  => m ()
ui = do

  ready <- getPostBuild
  areasE <- getAreas ready
  areasD <- holdDyn [] $ fmapMaybe reqSuccess areasE

  dyn_ $ do
    areas <- mkAreas <$> areasD
    return $ mdo
      state <- route' encodeRoute (decodeRoute areas) events
      events <- view areas state
      return ()



