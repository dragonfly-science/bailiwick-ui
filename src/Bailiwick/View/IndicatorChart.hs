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
import Language.Javascript.JSaddle (jsg2, obj, setProp, MonadJSM, JSM, liftJSM, ToJSVal, toJSVal, JSVal, JSString)
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

type ShapedData = [((AreaId, Text, Text, [Text], Maybe FeatureId), [(Year, Maybe Double, Maybe Double, Text, Text)])]
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

data ChartArgs
  = ChartArgs
    { chartArgsYear           :: Maybe Year
    , chartArgsIndictorId     :: Maybe Text
    , chartArgsTransform      :: Maybe Text
    , chartArgsAreaname       :: Maybe Text
    , chartArgsAreatype       :: Maybe Text
    , chartArgsChartType      :: Maybe Text
    , chartArgsFeatureId      :: Maybe Text
    , chartArgsZoom           :: Maybe Bool
    , chartArgsChartData      :: Maybe Chart
    , chartArgsChartCaption   :: Text
    , chartArgsFeatures       :: [(FeatureId, Text)]
    , chartArgsAreas          :: [Text]
    } deriving (Eq, Show)

instance ToJSVal ChartArgs where
  toJSVal ChartArgs{..} = do
    res <- obj
    let set :: ToJSVal a => JSString -> a -> JSM ()
        set key val = do
            valJS <- toJSVal val
            setProp key valJS res
    set "year"          chartArgsYear
    set "indictorId"    chartArgsIndictorId
    set "transform"     chartArgsTransform
    set "areaname"      chartArgsAreaname
    set "areatype"      chartArgsAreatype
    set "chartType"     chartArgsChartType
    set "featureId"     chartArgsFeatureId
    set "zoom"          chartArgsZoom
    set "chartData"     chartArgsChartData
    set "chartCaption"  chartArgsChartCaption
    set "features"      chartArgsFeatures
    set "areas"         chartArgsAreas
    toJSVal res

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

  (e, rightZoomE) <- divClass "chart-wrapper" $ do
    elAttr "div" ("class" =: "chart-inner") $ do

        rightZoomE <-
          elAttr "div" ("class" =: ("zoom-controls map-zoom " <> ("active"))) $ do
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

  let shapedDataD = do
        indn <- indicatorNumbersD
        areas <- areasD
        return (shapeData areas indn)

  let makeJSVal a = do
        val <- liftJSM $ toJSVal a
        return (Just val)
  shapedDataJSE <- performEvent (makeJSVal <$> updated shapedDataD)
  shapedDataJSD <- holdDyn Nothing shapedDataJSE

  let areanamesD = do
        areas <- areasD
        let hash = maybe OMap.empty unAreas areas
        return $ OMap.keys hash

      chartTypeD = fmap themePageRightChart <$> pageD

      chartD = do
        mindicator <- indicatorD
        mchartType <- chartTypeD
        return $ do
          Indicator{..} <- mindicator
          charts <- indicatorCharts
          chartid <- mchartType
          OMap.lookup chartid charts

      jsargsD = do
        my  <- fmap themePageYear <$> pageD
        indId <- fmap themePageIndicatorId <$> pageD
        transform <- fmap themePageLeftTransform <$> pageD
        areaType <- fmap themePageAreaType <$> pageD
        featureId <- fmap themePageFeatureId <$> pageD
        areanames <- areanamesD
        indicator <- indicatorD
        area <- areaD
        zoom <- zoomD
        page <- pageD
        chart <- chartD
        chartType <- chartTypeD

        let chartLabel = textSubstitution area Nothing indicator
                            (join featureId) page

        let label = case transform of
              Just t -> do
                let l = textLabel indicator t
                chartLabel l
              Nothing -> ""
        let areaname = maybe "" areaName area

        let features = case indicator of
                Just a ->
                    maybe [] OMap.toList (indicatorFeatureText a)
                Nothing -> []

        return $ ChartArgs
                      { chartArgsYear          = my
                      , chartArgsIndictorId    = (unIndicatorId <$> indId)
                      , chartArgsTransform     = transform
                      , chartArgsAreaname      = Just areaname
                      , chartArgsAreatype      = areaType
                      , chartArgsChartType     = (unChartId <$> chartType)
                      , chartArgsFeatureId     = (featureIdText <$> (join featureId))
                      , chartArgsZoom          = Just zoom
                      , chartArgsChartData     = chart
                      , chartArgsChartCaption  = label
                      , chartArgsFeatures      = features
                      , chartArgsAreas         = areanames
                      }


  jsargsJSD <- toJSValDyn jsargsD

  let getJSChartType chart = case chart of
        Just a -> case a of
                    "barchart"            -> "updateAreaBarchart"
                    "over-under-barchart" -> "overUnderBarchart"
                    "treemap"             -> "areaTreeMap"
                    _                     -> "updateIndicatorTimeSeries"
        Nothing ->                           "updateIndicatorTimeSeries"



  let jsargs = (,,) <$> shapedDataJSD <*> jsargsJSD <*> (getJSChartType <$> chartTypeD)
  let initialUpdate = tagPromptlyDyn jsargs readyE
  let updateValuesE = updated jsargs
  updateE :: Event t (Maybe JSVal, Maybe JSVal, JSString)
    <- switchHold initialUpdate (updateValuesE <$ readyE)

  performEvent_ $ ffor updateE $ \case
    (Just shapedData, Just args, jscharttype)
      -> liftJSM $ void $ jsg2 jscharttype (_element_raw e) (shapedData, args)
    _ -> return ()

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


