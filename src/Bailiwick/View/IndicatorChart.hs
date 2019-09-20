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

import Control.Monad (void)
import Control.Monad.Fix
import Data.Maybe (fromMaybe, isJust)
import Data.Functor.Compose (Compose(..))
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Map as Map
import Data.Text (Text, isPrefixOf)

import qualified GHCJS.DOM.Element as DOM
import Language.Javascript.JSaddle
   (jsg2, obj, setProp, MonadJSM, JSM, liftJSM, ToJSVal, toJSVal, JSString)
import Reflex.Dom.Core

import Bailiwick.Javascript
import Bailiwick.Route
import Bailiwick.Types
import Bailiwick.View.Text (textSubstitution)

data IndicatorChartState t
  = IndicatorChartState
    { areaD              :: Dynamic t (Loadable Area)
    , chartTypeD         :: Dynamic t (Loadable ChartId)
    , featureD           :: Dynamic t (Loadable (Maybe FeatureId))
    , transformD         :: Dynamic t (Loadable TransformId)
    , yearD              :: Dynamic t (Loadable Year)
    , areaTypeD          :: Dynamic t (Loadable AreaType)
    , areasD             :: Dynamic t (Loadable Areas)
    , compareAreaD       :: Dynamic t (Loadable (Maybe Area))
    , indicatorD         :: Dynamic t (Loadable Indicator)
    , indicatorNumbersD  :: Dynamic t (Loadable IndicatorNumbers)
    }

type ShapedData = [( (AreaId, Text, Text, [Text], Maybe FeatureId)
                   , [(Year, Maybe Double, Maybe Double, Text, Text)]
                   )]
shapeData :: Areas -> IndicatorNumbers -> ShapedData
shapeData (Areas areas) (IndicatorNumbers inmap) =
  let lookupAreaName areaid
        = fromMaybe "" $ do
            area <- OMap.lookup areaid areas
            return (areaName area)
      lookupAreaLevel areaid
        = fromMaybe "" $ do
            area <- OMap.lookup areaid areas
            return (areaLevel area)
      lookupAreaParents areaid
        = fromMaybe [] $ do
            area <- OMap.lookup areaid areas
            return (areaParents area)

      step (areaid, year, _mfeatureid) Numbers{..} res
         = do
          let feature = do
                f <- _mfeatureid
                return f
              key = (areaid, lookupAreaName areaid, lookupAreaLevel areaid, lookupAreaParents areaid, feature)
           in  OMap.alter (malter (year, rawNum, indexNum, headlineDisp, indexDisp)) key res
      malter yns Nothing = Just [yns]
      malter yns (Just this) = Just (yns:this)
  in  OMap.toList $ OMap.foldrWithKey step OMap.empty inmap

textLabel :: Loadable Indicator -> Text -> Text
textLabel ind transform =
  case ind of
    Loaded i -> do
        let config = indicatorLanguageConfig i
            captions = langLabels config

        case captions of
          Just c -> do
            case Map.lookup transform c of
              Just val -> val
              Nothing -> ""
          Nothing -> ""

    Loading -> "loading ... "
    Missing -> ""

data ChartArgs
  = ChartArgs
    { chartArgsYear           :: Year
    , chartArgsIndictorId     :: Text
    , chartArgsTransform      :: Text
    , chartArgsAreaname       :: AreaId
    , chartArgsAreatype       :: AreaType
    , chartArgsFeatureId      :: Maybe Text
    , chartArgsZoom           :: Bool
    , chartArgsChartData      :: Chart
    , chartArgsChartCaption   :: Text
    , chartArgsFeatures       :: [(FeatureId, Text)]
    , chartArgsAreas          :: [AreaId]
    , chartArgsCompareArea    :: Maybe AreaId
    }
    deriving (Show, Eq)

instance ToJSVal ChartArgs where
  toJSVal ChartArgs{..} = do
    res <- obj
    let set :: ToJSVal v => JSString -> v -> JSM ()
        set key val = do
            jsval <- toJSVal val
            setProp key jsval res
    set "year"          chartArgsYear
    set "indictorId"    chartArgsIndictorId
    set "transform"     chartArgsTransform
    set "areaname"      chartArgsAreaname
    set "areatype"      chartArgsAreatype
    set "featureId"     chartArgsFeatureId
    set "zoom"          chartArgsZoom
    set "chartData"     chartArgsChartData
    set "chartCaption"  chartArgsChartCaption
    set "features"      chartArgsFeatures
    set "areas"         chartArgsAreas
    set "compareArea"   chartArgsCompareArea
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
        areas <- areasD
        lNumbers <- indicatorNumbersD
        return (shapeData <$> areas <*> lNumbers)
  shapedDataJSD <- toJSValDynHold shapedDataD

  let areanamesD = do
        areas <- areasD
        return $ fmap (OMap.keys . unAreas) areas

  let chartD = do
        lindicator <- indicatorD
        lchartType <- chartTypeD
        return $ do
          chartid <- lchartType
          Indicator{..} <- lindicator
          toLoadable $ do
            charts <- indicatorCharts
            OMap.lookup chartid charts

      labelD = do
        indicator <- indicatorD
        area <- areaD
        feature <- featureD
        year <- yearD
        let chartLabel = textSubstitution
                            (toMaybe area)
                            Nothing
                            (toMaybe indicator)
                            (fromLoadable Nothing feature)
                            Nothing
                            (toMaybe year)
        mtransform <- transformD
        return $ do
          transform <- mtransform
          let l = textLabel indicator transform
          return $ chartLabel l

      featuresD = do
        lindicator <- indicatorD
        return $ do
          Indicator{..} <- lindicator
          toLoadable $ OMap.toList <$> indicatorFeatureText

  -- Using Compose to combine applicative instance for Dynamic t . Loadable
  let Compose jsargsD =
        ChartArgs
          <$> Compose yearD
          <*> Compose (fmap (unIndicatorId . indicatorId) <$> indicatorD)
          <*> Compose transformD
          <*> Compose (fmap areaName <$> areaD)
          <*> Compose areaTypeD
          <*> Compose (fmap (fmap featureIdText) <$> featureD)
          <*> Compose (Loaded <$> zoomD)
          <*> Compose chartD
          <*> Compose labelD
          <*> Compose featuresD
          <*> Compose areanamesD
          <*> Compose (fmap (fmap areaName) <$> compareAreaD)

  jsargsJSD <- toJSValDyn jsargsD

  let getJSChartType :: Loadable ChartId -> JSString
      getJSChartType chart = case chart of
        Loaded a -> case a of
                    "barchart"            -> "updateAreaBarchart"
                    "over-under-barchart" -> "overUnderBarchart"
                    "treemap"             -> "areaTreeMap"
                    _                     -> "updateIndicatorTimeSeries"
        _        ->                          "updateIndicatorTimeSeries"



  let jsargs = (,,) <$> shapedDataJSD <*> jsargsJSD <*> (getJSChartType <$> chartTypeD)
  let initialUpdate = tag (current jsargs) readyE
  let updateValuesE = updated jsargs
--  updateE :: Event t (Loadable JSVal, Loadable JSVal, JSString)
--      <- switchHold initialUpdate (updateValuesE <$ readyE)
  let updateE = leftmost [ initialUpdate, updateValuesE ]

  let showUpdateE (sd, jsa, js) =
        let showLoadable (Loaded _) = "Loaded _"
            showLoadable Missing    = "Missing"
            showLoadable Loading    = "Loading"
        in  "(" ++ showLoadable sd ++ ", " ++ showLoadable jsa ++ ", " ++ show js ++ ")"

  performEvent_ $ ffor (traceEventWith (showUpdateE) updateE) $ \case
    (Loaded shapedData, Loaded args, jscharttype)
      -> do liftJSM $ void $ jsg2 jscharttype (_element_raw e) (shapedData, args)
    _ -> do return ()

  clickE :: Event t (Maybe Message)
    <- clickEvents e $ \svg -> do
         target_element :: Text <- DOM.getTagName svg
         case target_element of
           "path" -> do
              yeart <- DOM.getAttribute svg ("data-bailiwick-year"::Text)
              area <- DOM.getAttribute svg ("data-bailiwick-area"::Text)
              let year = read <$> yeart
              return (SetYearArea <$> year <*> area)
           "rect" -> do
              area <- DOM.getAttribute svg ("data-bailiwick-area"::Text)
              feature <- DOM.getAttribute svg ("data-bailiwick-feature"::Text)
              if isJust feature && feature /= Just ""
                then return (SetFeature <$> feature)
                else if Just True == (isPrefixOf "auckland"  <$> area)
                  then return (SetSubArea "ward" <$> area)
                  else return (SetSubArea "ta" <$> area)
           "text" -> do
              yeart <- DOM.getAttribute svg ("data-bailiwick-year"::Text)
              let year = read <$> yeart
              return (SetYear <$> year)
           _ -> return Nothing

  return $ leftmost[ rightZoomE
                   , fmapMaybe id clickE ]


