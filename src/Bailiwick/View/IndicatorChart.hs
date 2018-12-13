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

import Control.Monad (join, void)
import Data.Maybe (isJust, maybe, fromMaybe, fromJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import Language.Javascript.JSaddle
       (jsg2, MonadJSM, liftJSM)
import Reflex.Dom.Core
import Reflex.PerformEvent.Class (PerformEvent(..))
import qualified GHCJS.DOM.Types as DOM (Element(..))

import Bailiwick.AreaTrees (AreaTrees)
import Bailiwick.State (getThemePage, ThemePageArgs(..), Message, State(..))
import Bailiwick.Types
       (display, elAttr', elDynAttr', elDynAttrNS, elDynAttrNS',
        GhcjsDomSpace, DomBuilderSpace, el, display, dynText, DomBuilder, elAttr,
        text, (=:), elClass, divClass, Dynamic, _element_raw, Event, PostBuild, never, getPostBuild)
import Reflex (Dynamic(..), TriggerEvent, constDyn, current, ffor, leftmost, tag, updated)
import Reflex.PerformEvent.Class (PerformEvent(..))
import GHCJS.DOM.Types (Element(..), HTMLElement(..))
import Text.Show.Pretty

import Bailiwick.View.IndicatorSummary (elDynHtml')
import Bailiwick.AreaTrees (AreaTrees, AreaTree)
import Bailiwick.State
       (getThemePage, ThemePageArgs(..), Message, State(..))
import Bailiwick.Store (getChartData)
import Bailiwick.Types

import qualified Data.Text as T (pack)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

prettyDisplay val = elDynHtml' "pre" $ T.pack . maybe "pretty show fail" valToStr . reify <$> val

indicatorChart
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => AreaTrees
  -> Indicators
  -> Dynamic t State
  -> m (Event t Message)
indicatorChart areaTrees indicators state = do
  postBuild <- getPostBuild
  let _year = fmap themePageYear . getThemePage <$> state
      _iId = fmap themePageIndicatorId . getThemePage <$> state
      _areaTree =
        (getThemePage <$> state) >>=
            mapM (\ themePage ->
                let y = themePageYear themePage
                    ind = themePageIndicatorId themePage
                in return $ OM.lookup (IndicatorId $ unIndicatorId ind
                                                   <> "-"
                                                   <> T.pack (show y))
                                      areaTrees)
      indicatorD = ((`OM.lookup` indicators) =<<) .
                     fmap themePageIndicatorId . getThemePage <$> state

  -- TODO: we now know the time series from the indicator,
  -- we just need to retrieve the current "chartD" json from the
  -- api (giving us a ChartData). Once we have the ChartData, we can then
  -- pass it on to the jsg2 call below.
  let chartD = (fromMaybe "none" . ((listToMaybe . indicatorTimeseries) =<<)
                 <$> indicatorD)
                 <> "-11d88bc13.json"
      -- chartDataD = postBuild <$> getChartData chartD

  let showAttr True  = "display: block"
      showAttr False = "display: none"
  (e, _) <- elDynAttr' "div" (constDyn $ "class" =: "basic-barchart") $
      elDynAttrNS
        (Just "http://www.w3.org/2000/svg") "svg"
        (constDyn $ "class"=:"d3-attach" <> "width"=:"480" <> "height"=:"530") $
          return ()


  -- Data to pass to chart:
  -- - Years?
  -- - indicator
  -- - area
  -- - caption
  -- - chartData
  -- - compareArea
  performEvent_ $ ffor (leftmost
                       [tag (current chartD) postBuild, updated chartD]
                       )
                       $ \chart -> do
    liftJSM $ jsg2 ("updateAreaBarchart" :: Text)
                   (_element_raw e :: Element)
                   (chart :: Text)
    return ()

  return never
