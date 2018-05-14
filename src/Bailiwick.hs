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

import Bailiwick.Route (decodeRoute, encodeRoute)
import Bailiwick.View (view)

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
  rec
      state <- route' encodeRoute decodeRoute events
      events <- view state
  return ()



