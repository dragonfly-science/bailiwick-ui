{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.Indicators (
  indicators
) where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Map.Ordered as OMap (lookup, assocs)
import Data.Foldable (forM_)

import GHCJS.DOM.Types (MonadJSM)

import Reflex (TriggerEvent, PerformEvent, PostBuild, Dynamic, Performable, never, Event)
import Reflex.Dom.Core
       ((=:), elAttr, elClass, dynText, text, el, divClass, GhcjsDomSpace,
        DomBuilder, DomBuilderSpace)

import Bailiwick.Types (Theme(..), Area(..), Indicator(..), Themes, Indicators)
import Bailiwick.State (getArea, Message, State(..))
import Data.Monoid ((<>))

indicators
  :: forall m t.
     ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Themes
  -> Indicators
  -> Dynamic t State
  -> m (Event t Message)
indicators themes inds state = do
  let dispArea = maybe "New Zealand" areaName . getArea <$> state
  divClass "themes-outer indicators-section" $ do
    elAttr "span" ("id" =: "indicators") $ return ()
    divClass "content" $
      divClass "themes" $ do
        divClass "theme-top" $
          divClass "section-header" $
            el "p" $ do
              el "i" $ return ()
              text "Indicators for "
              dynText dispArea
        divClass "theme-cards" $
          forM_ (OMap.assocs themes) $ \(k, Theme{..}) ->
            divClass "theme-card" $ do
              divClass ("card-header card-header-" <> k) $ do
                el "i" $ return ()
                text themeName
              divClass "card-copy" $
                el "ul" $
                  forM_ themeIndicators $ \iId ->
                    forM_ (OMap.lookup iId inds) $ \Indicator{..} ->
                      elClass "li" "inicator-list" $
                        text indicatorName
  return never
