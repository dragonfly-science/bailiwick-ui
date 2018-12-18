-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}
module Bailiwick.View.IndicatorChart (
  indicatorChart
) where

import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import GHCJS.DOM.Types (Element(..))
import Language.Javascript.JSaddle (jsg2, MonadJSM, liftJSM)
import Reflex.Dom.Core
       (elAttr, elDynAttr', GhcjsDomSpace, DomBuilderSpace, DomBuilder,
        (=:), Dynamic, _element_raw, Event, MonadHold, PostBuild, never, getPostBuild)
import Reflex (TriggerEvent, constDyn, current, delay, ffor, holdUniqDyn, leftmost, tag, traceEvent, updated)
import Reflex.PerformEvent.Class (PerformEvent(..))

import Bailiwick.Store (Store)
import qualified Bailiwick.Store as Store
import Bailiwick.State (getThemePage, ThemePageArgs(..), Message, State(..))
import Bailiwick.Types


indicatorChart
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Store
  -> Dynamic t State
  -> m (Event t Message)
indicatorChart storeD stateD = do
  let _year = fmap themePageYear . getThemePage <$> stateD
      _iId = fmap themePageIndicatorId . getThemePage <$> stateD
      _areaTree = do
         mThemePage <- getThemePage <$> stateD
         areaTrees <- Store.getAreaTrees <$> storeD
         return $ do
            themePage <- mThemePage
            let y = themePageYear themePage
                ind = themePageIndicatorId themePage
            OM.lookup (IndicatorId $ unIndicatorId ind <> "-" <> T.pack (show y))
                      areaTrees
      indicatorD' = do
         mThemePage <- getThemePage <$> stateD
         indicators <- Store.getIndicators <$> storeD
         return $ do
            themePage <- mThemePage
            OM.lookup (themePageIndicatorId themePage) indicators
  indicatorD <- holdUniqDyn indicatorD'

  -- TODO: we now know the time series from the indicator,
  -- we just need to retrieve the current "chartD" json from the
  -- api (giving us a ChartData). Once we have the ChartData, we can then
  -- pass it on to the jsg2 call below.
  let chartFilenameD = (fromMaybe "none" . ((listToMaybe . indicatorTimeseries) =<<)
                 <$> indicatorD)
                 <> "-11d88bc13.json"
  chartD <- Store.getChartData chartFilenameD

  let _showAttr True  = "display: block"
      _showAttr False = "display: none"
  (e, _) <- elDynAttr' "div" (constDyn $ "class" =: "basic-barchart") $
    elAttr "div" ("class"=:"d3-attach" <> "style"=:"width: 480px; height: 530px") $ return ()

  -- Data to pass to chart:
  -- - Years?
  -- - indicator
  -- - area
  -- - caption
  -- - chartData
  -- - compareArea
  delayEvent <- delay 0.5 =<< getPostBuild
  performEvent_ $ ffor (leftmost
                       [tag (current chartD) delayEvent, traceEvent "debugging event.."$ fmapMaybe id $ updated chartD]
                       )
                       $ \chart -> do
    _ <- liftJSM $ jsg2 ("updateAreaBarchart" :: Text)
                   (_element_raw e :: Element)
                   (chart :: Text)
    return ()

  return never
