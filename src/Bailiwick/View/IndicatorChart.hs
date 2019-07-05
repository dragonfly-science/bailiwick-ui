{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo         #-}
module Bailiwick.View.IndicatorChart
  ( indicatorChart
  , IndicatorChartState(..)
  , textLabel
) where

import Control.Monad (join, void)
import Control.Monad.Fix
import Data.Maybe (fromMaybe, isJust)
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Map as Map
import Data.Text (Text)
import Debug.Trace

import qualified GHCJS.DOM.Element as DOM
import Language.Javascript.JSaddle (jsg2, MonadJSM, liftJSM)
import Reflex.Dom.Core

import Bailiwick.Javascript
import Bailiwick.Route
import Bailiwick.Types
import Bailiwick.View.Text (textSubstitution)

data IndicatorChartState t
  = IndicatorChartState
    { routeD             :: Dynamic t Route
    , areaD              :: Dynamic t (Maybe Area)
    , areasD             :: Dynamic t (Maybe Areas)
    , featureD           :: Dynamic t (Maybe Feature)
    , indicatorD         :: Dynamic t (Maybe Indicator)
    , indicatorNumbersD  :: Dynamic t IndicatorNumbers
    }

type ShapedData = [((AreaId, Text, Text, [Text], Maybe FeatureId), [(Year, Text, Text, Text, Text)])]
shapeData :: Maybe Areas -> IndicatorNumbers -> ShapedData
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

textLabel :: Maybe Indicator -> Text -> Text
textLabel ind transform =
  case ind of
    Just i -> do
        let config = indicatorLanguageConfig i
            captions = langLabels config

        case captions of
          Just c -> do
            case Map.lookup transform c of
              Just val -> val
              Nothing -> ""
          Nothing -> ""

    Nothing -> ""

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
     , MonadFix m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => IndicatorChartState t
  -> Dynamic t Bool
  -> m (Event t Message)
indicatorChart IndicatorChartState{..} zoomD = do
  let pageD = getThemePage <$> routeD
      _chartType = fmap themePageRightChart <$> pageD

  (e, rightZoomE) <- divClass "chart-wrapper" $ do
    elAttr "div" ("class" =: "chart-inner") $ do
        rightZoomE <-
          divClass "zoom-controls map-zoom active" $ do
              let inpAttrD switchD = ffor switchD $ \case
                      True  -> ("type" =: "checkbox" <> "class" =: "checked")
                      False -> ("type" =: "checkbox")
              (eZoomIn, _) <-
                  el "label" $ do
                      elDynAttr "input" (inpAttrD zoomD) $ return ()
                      elClass' "span" "zoom-in" $ return ()
              (eZoomOut, _) <-
                  el "label" $ do
                      elDynAttr "input" (inpAttrD (not <$> zoomD)) $ return ()
                      elClass' "span" "zoom-out" $ return ()

              return $
                    leftmost [ RightZoomIn <$ domEvent Click eZoomIn
                             , RightZoomOut Nothing <$ domEvent Click eZoomOut ]

        (e, _) <- elAttr' "div" ("class" =: "d3-attach") $ return ()
        divClass "tooltip" $ return ()
        divClass "legend" $ return ()
        return (e, rightZoomE)

  readyE <- getPostBuild

  let _year = fmap themePageYear <$> pageD
      _iId = fmap themePageIndicatorId <$> pageD
      _transform = fmap themePageLeftTransform <$> pageD
      _areaType = fmap themePageAreaType <$> pageD
      _featureId = fmap themePageFeatureId <$> pageD

      shapedDataD = do
        indn <- indicatorNumbersD
        areas <- areasD
        return (shapeData areas indn)

      areanamesD = do
        areas <- areasD
        let hash = maybe OMap.empty unAreas areas
        return $ OMap.keys hash
      jsargs = do
        shapedData <- shapedDataD
        areanames <- areanamesD
        my <- _year
        indID <- _iId
        area <- areaD
        transform <- _transform
        areatype <- _areaType
        chartType <- _chartType
        indicator <- indicatorD
        featureId <- _featureId
        zoom <- zoomD
        page <- pageD

        let chartLabel = textSubstitution area Nothing indicator
                            (join featureId) page

        let label = case transform of
              Just t -> do
                let l = textLabel indicator t
                chartLabel l
              Nothing -> ""

        return (my, indID, indicator, shapedData, areanames, area, areatype, transform, chartType, join featureId, zoom, label)

  let initialUpdate = tagPromptlyDyn jsargs readyE
  let updateValuesE = updated jsargs
  updateE :: Event t (Maybe Year, Maybe IndicatorId,
                      Maybe Indicator, ShapedData, [AreaId], Maybe Area, Maybe Text,
                      Maybe Text, Maybe ChartId, Maybe FeatureId, Bool, Text)
    <- switchHold initialUpdate (updateValuesE <$ readyE)


  let getJSChartType chart = case chart of
        Just a -> case a of
                    "barchart"            -> "updateAreaBarchart"
                    "over-under-barchart" -> "overUnderBarchart"
                    "treemap"             -> "areaTreeMap"
                    _                     -> "updateIndicatorTimeSeries"
        Nothing ->                           "updateIndicatorTimeSeries"

  performEvent_ $ ffor updateE $ \case
    (my, indID, indicator, shapedData, areanames, area, areatype, transform, chartType, featureId, zoom, label)
      -> liftJSM . void
          $ do
            let areaname = maybe "" areaName area
            let features = case indicator of
                    Just a ->
                        maybe [] OMap.toList (indicatorFeatureText a)
                    Nothing -> []
            let chart = do
                    Indicator{..} <- indicator
                    charts <- indicatorCharts
                    chartid <- chartType
                    OMap.lookup chartid charts
            let zoomed = case zoom of
                    True -> "true"
                    False -> "false"
            -- chartLabel text...
            args <- makeJSObject
                     [ ("indictorId",  (unIndicatorId <$> indID))
                     , ("transform",   transform)
                     , ("areaname",    Just areaname)
                     , ("areatype",    areatype)
                     , ("chartType",   (unChartId <$> chartType))
                     , ("featureId",   (featureIdText <$> featureId))
                     , ("zoom",        Just zoomed)
                     ]
            jsg2 ((getJSChartType chartType) :: Text) (_element_raw e)
                 (shapedData, my, args, features, chart, label, areanames)

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
              feature <- DOM.getAttribute svg ("data-bailiwick-feature"::Text)
              if isJust feature
                then return (SetFeature <$> feature)
                else return (SetSubArea <$> area)
           "text" -> do
              yeart <- DOM.getAttribute svg ("data-bailiwick-year"::Text)
              let year = read <$> yeart
              return (SetYear <$> year)
           _ -> return Nothing

  return $ leftmost[ rightZoomE
                   , fmapMaybe id clickE ]


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
