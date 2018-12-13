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

import Bailiwick.Store (getAreas, getAreaSummaries, getThemes, getIndicators, getAreaTrees, getFeatures)
import Bailiwick.Route (decodeRoute, encodeRoute)
import Bailiwick.View (view)
import Bailiwick.Types
import Bailiwick.AreaTrees hiding (areas)

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
  let maybeGetList = holdDyn [] . fmapMaybe reqSuccess
  areasD <- fmap (fmap mkAreas) $ maybeGetList =<< getAreas ready
  areaSummariesD <- fmap (fmap mkAreaSummaries) $ maybeGetList =<< getAreaSummaries ready
  themesD <- fmap (fmap mkThemes) $ maybeGetList =<< getThemes ready
  indicatorsD <- fmap (fmap mkIndicators) $ maybeGetList =<< getIndicators ready
  areaTreesD <- fmap (fmap mkAreaTrees) $ maybeGetList =<< getAreaTrees ready
  featuresD <- fmap (fmap mkFeatures) $ maybeGetList =<< getFeatures ready

  dyn_ $ do
    areas <- areasD
    areaSummaries <- areaSummariesD
    themes <- themesD
    indicators <- indicatorsD
    areaTrees <- areaTreesD
    features <- featuresD
    return $ mdo
      state <- route' (encodeRoute areas) (decodeRoute areas) events
      events <- view areas areaSummaries themes indicators areaTrees features state
      return ()

