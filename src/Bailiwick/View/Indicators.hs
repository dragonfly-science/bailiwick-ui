{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.Indicators (
  indicators
) where

import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict.InsOrd as OMap (lookup, toList)
import Data.Traversable (forM)

import GHCJS.DOM.Types (MonadJSM)

import Reflex
       (leftmost, TriggerEvent, PerformEvent, MonadHold, PostBuild, Dynamic,
        Performable, Event, switchHold, never)
import Reflex.Dom.Core
       (elClass', (=:), elAttr, text, el, divClass, dyn,
        GhcjsDomSpace, DomBuilder, DomBuilderSpace, domEvent, EventName(Click))

import Bailiwick.Store (Store)
import qualified Bailiwick.Store as Store
import Bailiwick.Types (Theme(..), Area(..), Indicator(..))
import Bailiwick.State
       (ThemePageArgs(..), Page(..), Message(..), getArea, Message,
        State(..))

indicators
  :: forall m t.
     ( Monad m
     , PostBuild t m
     , MonadHold t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Dynamic t Store
  -> Dynamic t State
  -> m (Event t Message)
indicators storeD stateD = (switchHold never =<<) . dyn $ do
  marea <- getArea <$> stateD
  themes <- Store.getThemes <$> storeD
  inds <- Store.getIndicators <$> storeD
  return $
    divClass "themes-outer indicators-section" $ do
      elAttr "span" ("id" =: "indicators") $ return ()
      divClass "content" $
        divClass "themes" $ do
          divClass "theme-top" $
            divClass "section-header" $
              el "p" $ do
                el "i" $ return ()
                text "Indicators for "
                text $ maybe "New Zealand" areaName marea
          divClass "theme-cards" $
            fmap leftmost . forM (OMap.toList themes) $ \(k, Theme{..}) ->
              divClass "theme-card" $ do
                divClass ("card-header card-header-" <> k) $ do
                  el "i" $ return ()
                  text themeName
                divClass "card-copy" $
                  el "ul" $
                    fmap leftmost . forM themeIndicators $ \iId ->
                      fmap (leftmost . maybeToList) . forM (OMap.lookup iId inds) $ \Indicator{..} -> do
                        click <- fmap (domEvent Click . fst) . elClass' "li" "inicator-list" $
                          text indicatorName
                        return $ GoTo (ThemePage $ ThemePageArgs
                              indicatorId
                              indicatorDefaultChartLeft
                              indicatorDefaultChartRight
                              2017 Nothing Nothing "reg" "indexed" "indexed") <$ click
