{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.IndicatorSummary (
    indicatorSummary
) where

import Reflex (PerformEvent, PostBuild, TriggerEvent)
import Reflex.Dom.Core
       (text, divClass, GhcjsDomSpace, DomBuilder, DomBuilderSpace)
import Language.Javascript.JSaddle (MonadJSM)
import Bailiwick.Types
       (AreaSummaries, Areas, Indicators)
import Reflex (Event(..), Dynamic(..), Performable)
import Bailiwick.State (Message, State(..))

indicatorSummary
  :: forall m t.
     ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Areas
  -> AreaSummaries
  -> Indicators
  -> Dynamic t State
  -> m (Event t Message)
indicatorSummary areas areaSummaries indicators state = do
  divClass "summary" $ do
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        text "TODO"
      divClass "intersection-number regional-value" $ do
        text "TODO"
      divClass "intersection-number national-value" $ do
        text "TODO"
  divClass "summary-links" $ do
    divClass "source" $ do
      text "TODO"
    divClass "notes" $ do
      text "TODO"
  divClass "table-view hide" $ do
        text "TODO"
  divClass "tabble-button" $ do
        text "TODO"
  return never

