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

import Control.Monad (void)
import Data.Text (Text, pack)
import Debug.Trace

import Reflex
import Reflex.Dom.Core
import Language.Javascript.JSaddle (jsg1, MonadJSM, liftJSM, valToObject)

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
  let jsargs = do
        area <- areaD
        transform <- _transform
        indicator <- indicatorD
        featureId <- featureD
        page <- pageD
        inputValues <- inputValuesD

        let label = mapLegendLabel 
                        area
                        indicator
                        featureId
                        transform
                        page

        return (inputValues, label)

  let initialUpdate = tagPromptlyDyn jsargs readyE
      width  = 481 :: Int
      height = 120 :: Int

  let updateValuesE = updated jsargs
  updateE :: Event t (Maybe (Double, Double), Text)
    <- switchHold initialUpdate (updateValuesE <$ readyE)
    
--   performEvent $ ffor (fmapMaybeCheap id updateE) $ \(minval, maxval) ->
--     liftJSM $ do
      
--       scaleVal <- jsg4 ("updateMapLegend" :: Text) width height minval maxval
--       scale <- valToObject scaleVal
--       return (ScaleFunction scale)
  performEvent $ ffor updateE $ \case
    (inputValues, label) 
      -> liftJSM $ do
        let range = case inputValues of
                Just a -> a
                Nothing -> (0.0, 0.0)

        args <- makeJSObject
                    [ ("min",    Just (pack $ (show (fst range))))
                    , ("max",    Just (pack $ (show (snd range))))
                    , ("width",  Just (pack $ (show width)))
                    , ("height", Just (pack $ (show height)))
                    , ("label",  Just label)
                    ]
        
        scaleVal <- jsg1 ("updateMapLegend" :: Text) args
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
