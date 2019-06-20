{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.IndicatorChart
  ( indicatorChart
  , IndicatorChartState(..)
) where

import Control.Monad (join, void)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Data.Text (Text)
import Debug.Trace

import qualified GHCJS.DOM.Element as DOM
import Language.Javascript.JSaddle (jsg2, MonadJSM, liftJSM, toJSValListOf)
import Reflex.Dom.Core

import Bailiwick.Javascript
import Bailiwick.Route
import Bailiwick.Types

data IndicatorChartState t
  = IndicatorChartState
    { routeD             :: Dynamic t Route
    , areaD              :: Dynamic t (Maybe Area)
    , areasD             :: Dynamic t (Maybe Areas)
    , featureD           :: Dynamic t (Maybe Feature)
    , indicatorD         :: Dynamic t (Maybe Indicator)
    , indicatorNumbersD  :: Dynamic t IndicatorNumbers
    }

shapeData :: Maybe Areas -> IndicatorNumbers -> [((AreaId, Text, Text, [Text], Maybe FeatureId), [(Year, Text, Text, Text, Text)])]
shapeData mareas (IndicatorNumbers inmap) =
  let lookupAreaName areaid
        = fromMaybe "" $ do
            Areas areas <- mareas
            area <- OMap.lookup areaid areas
            return (areaName area)
      lookupAreaLevel areaid
        = fromMaybe "" $ do
            Areas areas <- mareas
            area <- OMap.lookup areaid areas
            return (areaLevel area)
      lookupAreaParents areaid
        = fromMaybe [] $ do
            Areas areas <- mareas
            area <- OMap.lookup areaid areas
            return (areaParents area)
    --   lookupFeature featureId
    --     = do
    --         return OMap.lookup featureId

      step (areaid, year, _mfeatureid) Numbers{..} res
         = do
          let featureId = do
                f <- _mfeatureid
                return f
              key = (areaid, lookupAreaName areaid, lookupAreaLevel areaid, lookupAreaParents areaid, featureId)
           in  OMap.alter (malter (year, rawNum, indexNum, headlineDisp, indexDisp)) key res
      malter yns Nothing = Just [yns]
      malter yns (Just this) = Just (yns:this)
  in  OMap.toList $ OMap.foldrWithKey step OMap.empty inmap

indicatorChart
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , MonadJSM m
     , MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => IndicatorChartState t
  -> Dynamic t Bool
  -> m (Event t Message)
indicatorChart IndicatorChartState{..} zoomD = do
  (e, _) <- divClass "chart-wrapper" $ do
    elAttr' "div" ("class" =: "chart-inner") $ do
    --   _ <- divClass "zoom-controls map-zoom active" $ do
    --     let inpAttrD switchD = ffor switchD $ \case
    --             True  -> ("type" =: "checkbox" <> "class" =: "checked")
    --             False -> ("type" =: "checkbox")
    --     (eZoomIn, _) <- el "label" $ do
    --         elAttr "input" (inpAttrD zoomD) $
    --           return ()
    --         elClass' "span" "zoom-in" $
    --           return ()
    --     (eZoomOut, _) <- el "label" $ do
    --         elAttr "input" (inpAttrD (not <$> zoomD)) $
    --           return ()
    --         elClass' "span" "zoom-out" $
    --           return ()
    --     return $ leftmost [ tagPromptlyDyn
    --                             (RightZoomOut . fmap areaId <$> areaD)
    --                             (domEvent Click eZoomOut)
    --                       , RightZoomIn <$ domEvent Click eZoomIn
    --                       ]
      divClass "d3-attach" $ return ()
      divClass "tooltip" $ return ()
      divClass "legend" $ return ()
  readyE <- getPostBuild

  let pageD = getThemePage <$> routeD
      _year = fmap themePageYear <$> pageD
      _iId = fmap themePageIndicatorId <$> pageD
      _transform = fmap themePageLeftTransform <$> pageD
      _areaType = fmap themePageAreaType <$> pageD
      _chartType = fmap themePageRightChart <$> pageD
      _featureId = fmap themePageFeatureId <$> pageD

      jsargs = do
        indn <- indicatorNumbersD
        areas <- areasD
        my <- _year
        indID <- _iId
        area <- areaD
        transform <- _transform
        areatype <- _areaType
        chartType <- _chartType
        indicator <- indicatorD
        featureId <- _featureId

        let charts = case indicator of
                Just a -> indicatorCharts a
                Nothing -> []

        let mapping = chartMapping <$> charts

        return (indn, my, indID, indicator, trace ("mapping: " ++ show mapping) mapping, areas, area, areatype, transform, chartType, join featureId)

  let initialUpdate = tagPromptlyDyn jsargs readyE
  let updateValuesE = updated jsargs
  updateE :: Event t (IndicatorNumbers, Maybe Year, Maybe IndicatorId,
                      Maybe Indicator, [Maybe [ChartMapping]], Maybe Areas, Maybe Area, Maybe Text,
                      Maybe Text, Maybe ChartId, Maybe FeatureId)
    <- switchHold initialUpdate (updateValuesE <$ readyE)


  let getJSChartType chart = case chart of
        Just a -> case trace ("chart type" ++ show a) a of
                    "barchart"            -> "updateAreaBarchart"
                    "over-under-barchart" -> "overUnderBarchart"
                    "treemap"             -> "areaTreeMap"
                    _                     -> "updateIndicatorTimeSeries"
        Nothing ->                           "updateIndicatorTimeSeries"

  performEvent_ $ ffor updateE $ \case
    (indn, my, indID, indicator, mapping, areas, area, areatype, transform, chartType, featureId)
      -> liftJSM . void
          $ do
            let areaname = maybe "" areaName area
            let units = maybe Percentage indicatorUnits indicator
            let features = case indicator of
                    Just a -> indicatorFeatures a
                    Nothing -> []
            -- let chartMapping = fmap mappingLabel <$> mapping
            args <- makeJSObject
                     [ ("indictorId",  (unIndicatorId <$> indID))
                     , ("transform",   transform)
                     , ("areaname",    Just areaname)
                     , ("areatype",    areatype)
                     , ("chartType",   (unChartId <$> chartType))
                     , ("featureId",   (featureIdText <$> featureId))
                     ]
            jsg2 ((getJSChartType chartType) :: Text) (_element_raw e)
                 (shapeData areas indn, my, args, features)

  clickE :: Event t (Maybe Message)
    <- clickEvents e $ \svg -> do
         target_element :: Text <- DOM.getTagName svg
         case trace ("El" ++ show target_element) target_element of
           "path" -> do
              yeart <- DOM.getAttribute svg ("data-bailiwick-year"::Text)
              area <- DOM.getAttribute svg ("data-bailiwick-area"::Text)
              let year = read <$> yeart
              return (SetYearArea <$> year <*> area)
           "rect" -> do
              area <- DOM.getAttribute svg ("data-bailiwick-area"::Text)
            --   feature <- DOM.getAttribute svg ("data-bailiwick-feature"::Text)

              return (SetSubArea <$> area)
           "text" -> do
              yeart <- DOM.getAttribute svg ("data-bailiwick-year"::Text)
              let year = read <$> yeart
              return (SetYear <$> year)
           _ -> return Nothing

  return $ fmapMaybe id clickE


  -- TODO: we now know the time series from the indicator,
  -- we just need to retrieve the current "chartD" json from the
  -- api (giving us a ChartData). Once we have the ChartData, we can then
  -- pass it on to the jsg2 call below.
--   let chartFilenameD = (fromMaybe "none" . ((listToMaybe . indicatorTimeseries) =<<)
--                  <$> indicatorD)
--                  <> "-11d88bc13.json"
--   chartD <- State.getChartData $ traceDyn "chartFilenameD" chartFilenameD

  -- let _showAttr True  = "display: block"
  --     _showAttr False = "display: none"
  -- (_e, _) <- elDynAttr' "div" (constDyn $ "class" =: "basic-barchart") $
  --   elAttr "div" ("class"=:"d3-attach" <> "style"=:"width: 480px; height: 530px") $ return ()

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


  -- return never
