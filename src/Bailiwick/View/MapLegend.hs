{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.MapLegend
  ( mapLegend
  , MapLegendState(..)
  )
where

import Control.Monad (join)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Data.Text (Text, pack)

import Language.Javascript.JSaddle (jsg2, MonadJSM, liftJSM, valToObject)
import Reflex
import Reflex.Dom.Core

import Bailiwick.Javascript
import Bailiwick.Route
import Bailiwick.Types
import Bailiwick.View.IndicatorChart (textLabel)
import Bailiwick.View.Text (textSubstitution)

data MapLegendState t
  = MapLegendState
    { inputValuesD       :: Dynamic t (Maybe (Double, Double))
    , routeD             :: Dynamic t Route
    , areaD              :: Dynamic t (Maybe Area)
    , featureD           :: Dynamic t (Maybe FeatureId)
    , indicatorD         :: Dynamic t (Maybe Indicator)
    }

mapLegend
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => MapLegendState t
  -> m (Event t ScaleFunction)
mapLegend MapLegendState{..} = do
  readyE <- getPostBuild
  
  let pageD = getThemePage <$> routeD
  let _transform = fmap themePageLeftTransform <$> pageD
  let _chartType = fmap themePageLeftChart <$> pageD
  let jsargs = do
        area <- areaD
        transform <- _transform
        indicator <- indicatorD
        featureId <- featureD
        page <- pageD
        inputValues <- inputValuesD
        chartType <- _chartType

        let label = mapLegendLabel 
                        area
                        indicator
                        featureId
                        transform
                        page

        return (inputValues, label, indicator, chartType, transform)

  let initialUpdate = tagPromptlyDyn jsargs readyE
      width  = 481 :: Int
      height = 120 :: Int

  let updateValuesE = updated jsargs
  updateE :: Event t (Maybe (Double, Double), Text, Maybe Indicator, Maybe ChartId, Maybe Text)
    <- switchHold initialUpdate (updateValuesE <$ readyE)

  performEvent $ ffor updateE $ \case
    (inputValues, label, indicator, chartType, transform) 
      -> liftJSM $ do
        let range = case inputValues of
                Just a -> a
                Nothing -> (0.0, 0.0)
        
        let chart = do
                Indicator{..} <- indicator
                charts <- indicatorCharts
                chartid <- chartType
                OMap.lookup chartid charts

        args <- makeJSObject
                    [ ("min",    Just (pack $ (show (fst range))))
                    , ("max",    Just (pack $ (show (snd range))))
                    , ("width",  Just (pack $ (show width)))
                    , ("height", Just (pack $ (show height)))
                    , ("label",  Just label)
                    , ("transform", transform)
                    ]
        
        scaleVal <- jsg2 ("updateMapLegend" :: Text) args chart
        scale <- valToObject scaleVal
        return (ScaleFunction scale)

mapLegendLabel 
  :: Maybe Area
  -> Maybe Indicator
  -> Maybe FeatureId
  -> Maybe Text
  -> Maybe ThemePageArgs
  -> Text
mapLegendLabel area indicator featureId transform page = do
    let chartLabel = textSubstitution 
                        area 
                        Nothing 
                        indicator 
                        featureId 
                        page
            
    let label = case transform of
            Just tr -> textLabel indicator tr
            Nothing -> ""
    
    chartLabel label
