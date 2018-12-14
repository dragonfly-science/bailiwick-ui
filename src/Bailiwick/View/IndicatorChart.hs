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

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import GHCJS.DOM.Types (Element(..))
import Language.Javascript.JSaddle (jsg2, MonadJSM, liftJSM)
import Reflex.Dom.Core
       (elDynAttr', elDynAttrNS, GhcjsDomSpace, DomBuilderSpace, DomBuilder,
        (=:), Dynamic, _element_raw, Event, PostBuild, never, getPostBuild)
import Reflex (TriggerEvent, constDyn, current, ffor, leftmost, tag, updated)
import Reflex.PerformEvent.Class (PerformEvent(..))

import Bailiwick.AreaTrees (AreaTrees)
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

  let _showAttr True  = "display: block"
      _showAttr False = "display: none"
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
    _ <- liftJSM $ jsg2 ("updateDefaultTimeSeries" :: Text)
                   (_element_raw e :: Element)
                   (chart :: Text)
    return ()

  return never
