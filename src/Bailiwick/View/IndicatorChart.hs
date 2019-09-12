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
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Map as Map
import Data.Text (Text, isPrefixOf)

import qualified GHCJS.DOM.Element as DOM
import Language.Javascript.JSaddle
   (jsg2, obj, setProp, MonadJSM, JSM, liftJSM, ToJSVal, toJSVal, JSVal, valNull, JSString)
import Reflex.Dom.Core

import Bailiwick.Javascript
import Bailiwick.Route
import Bailiwick.Types
import Bailiwick.View.Text (textSubstitution)

data IndicatorChartState t
  = IndicatorChartState
    { areaD              :: Dynamic t (Maybe Area)
    , chartTypeD         :: Dynamic t (Maybe ChartId)
    , featureD           :: Dynamic t (Maybe FeatureId)
    , transformD         :: Dynamic t (Maybe TransformId)
    , yearD              :: Dynamic t (Maybe Year)
    , areaTypeD          :: Dynamic t (Maybe AreaType)
    , areasD             :: Dynamic t (Maybe Areas)
    , compareAreaD       :: Dynamic t (Maybe Area)
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
          let feature = do
                f <- _mfeatureid
                return f
              key = (areaid, lookupAreaName areaid, lookupAreaLevel areaid, lookupAreaParents areaid, feature)
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
    , chartArgsAreaname       :: Maybe AreaId
    , chartArgsAreatype       :: Maybe AreaType
    , chartArgsFeatureId      :: Maybe Text
    , chartArgsZoom           :: Maybe Bool
    , chartArgsChartData      :: Maybe Chart
    , chartArgsChartCaption   :: Maybe Text
    , chartArgsFeatures       :: Maybe [(FeatureId, Text)]
    , chartArgsAreas          :: Maybe [AreaId]
    , chartArgsCompareArea    :: Maybe AreaId
    }
    deriving Show

instance ToJSVal ChartArgs where
  toJSVal ChartArgs{..} = do
    res <- obj
    let set :: ToJSVal v => JSString -> Maybe v -> JSM ()
        set key val = do
            jsval <- mapM toJSVal val
            setProp key (fromMaybe valNull jsval) res
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

toJSValDynHold
  :: ( Eq val
     , Show val
     , ToJSVal val
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , MonadJSM m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t (Maybe val)
  -> m (Dynamic t (Maybe JSVal))
toJSValDynHold valD = do
  valDU <- holdUniqDyn valD
  toJSValDyn valDU

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

  let shapedDataD = shapeData <$> areasD <*> indicatorNumbersD
  let emptyToNothing [] = Nothing
      emptyToNothing as = Just as
  shapedDataJSD <- toJSValDynHold $ (emptyToNothing <$> shapedDataD)

  let areanamesD = do
        areas <- areasD
        let hash = maybe OMap.empty unAreas areas
        return $ OMap.keys hash

  let chartD = do
        mindicator <- indicatorD
        mchartType <- chartTypeD
        return $ do
          Indicator{..} <- mindicator
          charts <- indicatorCharts
          chartid <- mchartType
          OMap.lookup chartid charts

      labelD = do
        indicator <- indicatorD
        area <- areaD
        feature <- featureD
        year <- yearD
        let chartLabel = textSubstitution (toLoadable area) Missing (toLoadable indicator) feature Nothing year
        mtransform <- transformD
        return $ do
          transform <- mtransform
          let l = textLabel indicator transform
          return $ chartLabel l


  let jsargsD = Just <$> (ChartArgs
                           <$> yearD
                           <*> (fmap (unIndicatorId . indicatorId) <$> indicatorD)
                           <*> transformD
                           <*> (fmap areaName <$> areaD)
                           <*> areaTypeD
                           <*> (fmap featureIdText <$> featureD)
                           <*> (Just <$> zoomD)
                           <*> chartD
                           <*> labelD
                           <*> (fmap OMap.toList . (indicatorFeatureText =<<) <$> indicatorD)
                           <*> (Just <$> areanamesD)
                           <*> (fmap areaName <$> compareAreaD)
                          )

  let chartArgsComplete Nothing = False
      chartArgsComplete (Just ChartArgs{..})
        = let test = [ isJust chartArgsYear
                     , isJust chartArgsIndictorId
                     , isJust chartArgsTransform
                     , isJust chartArgsAreaname
                     , isJust chartArgsAreatype
                     , isJust chartArgsZoom
                     , isJust chartArgsChartData
                     , isJust chartArgsChartCaption
                     , isJust chartArgsAreas
                     ]
          in all id test

  jsargsJSD <- toJSValFilterDyn chartArgsComplete jsargsD

  let getJSChartType :: Maybe ChartId -> JSString
      getJSChartType chart = case chart of
        Just a -> case a of
                    "barchart"            -> "updateAreaBarchart"
                    "over-under-barchart" -> "overUnderBarchart"
                    "treemap"             -> "areaTreeMap"
                    _                     -> "updateIndicatorTimeSeries"
        Nothing ->                           "updateIndicatorTimeSeries"



  let jsargs = (,,) <$> shapedDataJSD <*> jsargsJSD <*> (getJSChartType <$> chartTypeD)
  let initialUpdate = tag (current jsargs) readyE
  let updateValuesE = updated jsargs
  updateE :: Event t (Maybe JSVal, Maybe JSVal, JSString)
      <- switchHold initialUpdate (updateValuesE <$ readyE)

  performEvent_ $ ffor updateE $ \case
    (Just shapedData, Just args, jscharttype)
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
              if isJust feature
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


