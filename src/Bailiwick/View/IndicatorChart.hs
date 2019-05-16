-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}
module Bailiwick.View.IndicatorChart
  ( indicatorChart
  , IndicatorChartState(..)
) where

import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))

--import GHCJS.DOM.Types (Element(..))
import Language.Javascript.JSaddle (MonadJSM) --(jsg2, MonadJSM, liftJSM)
--import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.Dom.Core hiding (Element)

import Bailiwick.Route
import Bailiwick.Types

data IndicatorChartState t
  = IndicatorChartState
    { routeD             :: Dynamic t Route
    , areaD              :: Dynamic t (Maybe Area)
    , featureD           :: Dynamic t (Maybe Feature)
    , indicatorD         :: Dynamic t (Maybe Indicator)
    , indicatorNumbersD  :: Dynamic t IndicatorNumbers
    }


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
     , HasJSContext (Performable m)
     )
  => IndicatorChartState t
  -> m (Event t Message)
indicatorChart IndicatorChartState{..} = do
  -- readyE <- getPostBuild
  let pageD = getThemePage <$> routeD
      _year = fmap themePageYear <$> pageD
      _iId = fmap themePageIndicatorId <$> pageD

  -- TODO: we now know the time series from the indicator,
  -- we just need to retrieve the current "chartD" json from the
  -- api (giving us a ChartData). Once we have the ChartData, we can then
  -- pass it on to the jsg2 call below.
--   let chartFilenameD = (fromMaybe "none" . ((listToMaybe . indicatorTimeseries) =<<)
--                  <$> indicatorD)
--                  <> "-11d88bc13.json"
--   chartD <- State.getChartData $ traceDyn "chartFilenameD" chartFilenameD

  let _showAttr True  = "display: block"
      _showAttr False = "display: none"
  (_e, _) <- elDynAttr' "div" (constDyn $ "class" =: "basic-barchart") $
    elAttr "div" ("class"=:"d3-attach" <> "style"=:"width: 480px; height: 530px") $ return ()

--   -- Data to pass to chart:
--   -- - Years?
--   -- - indicator
--   -- - area
--   -- - caption
--   -- - chartData
--   -- - compareArea
--   delayE <- delay 2.0 =<< getPostBuild
--   performEvent_ $ ffor (leftmost
--                        [ attachWithMaybe (flip $ const id) (current chartD) delayE
--                        , traceEventWith (("HERE: "++) . Prelude.take 100 . show) $ fmapMaybe id $ updated chartD]
--                        )
--                        $ \chart -> do
--     _ <- liftJSM $ jsg2 ("updateAreaBarchart" :: Text)
--                    (_element_raw e :: Element)
--                    (Array $ chartDataValues chart)
--     return ()

-- updateIndicatorTimeSeries
-- 
--   let initialUpdate = tagPromptlyDyn inputValuesD readyE
--   let updateValuesE = updated inputValuesD
--   updateE <- switchHold initialUpdate (updateValuesE <$ readyE)
--   performEvent_ $ ffor updateE $ \case
--     Just (d, area) -> liftJSM . void $ do
--          jsg3 ("updateTimeSeries" :: Text) (_element_raw e) d (maybe "" areaId area)
--     _ -> return ()
  return never
