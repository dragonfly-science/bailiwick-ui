{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.MapLegend ( mapLegend )
where

import Control.Monad.Fix
import qualified Data.HashMap.Strict.InsOrd as OMap
import Data.Text (Text, pack)

import Language.Javascript.JSaddle (jsg3, MonadJSM, liftJSM)
import Reflex
import Reflex.Dom.Core

import Bailiwick.Javascript
import Bailiwick.State
       (State(State, transformD, chartTypeD, indicatorD, featureD, yearD),
        getScaleExtentD)
import Bailiwick.Types
import Bailiwick.View.IndicatorChart (textLabel)
import Bailiwick.View.Text (textSubstitution)

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
     , MonadFix m
     )
  => State t
  -> m ()
mapLegend state = do
  (e, _) <- elAttr' "div" ("class" =: "legend indicator-map-legend") $ return ()

  readyE <- getPostBuild

  let inputValuesD = getScaleExtentD state
      State{indicatorD,yearD,featureD,transformD,chartTypeD} = state
      jsargs = do
        transform <- transformD
        chartType <- chartTypeD
        indicator <- indicatorD
        featureId <- featureD
        year <- yearD
        inputValues <- inputValuesD

        let label = mapLegendLabel
                        indicator
                        featureId
                        transform
                        year

        return (inputValues, label, indicator, chartType, transform)

  ujsargs <- holdUniqDyn jsargs
  let initialUpdate = tagPromptlyDyn ujsargs readyE
      width  = 481 :: Int
      height = 120 :: Int

  let updateValuesE = updated ujsargs
  updateE :: Event t (Maybe (Double, Double), Text, Loadable Indicator, Maybe ChartId, Maybe Text)
    <- switchHold initialUpdate (updateValuesE <$ readyE)

  performEvent_ $ ffor updateE $ \case
    (inputValues, label, indicator, chartType, transform)
      -> liftJSM $ do
        let range = case inputValues of
                Just a -> a
                Nothing -> (0.0, 0.0)

        let chart = do
                Indicator{..} <- toMaybe indicator
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

        _ <- jsg3 ("updateMapLegend" :: Text) (_element_raw e) args chart
        return ()

mapLegendLabel
  :: Loadable Indicator
  -> Maybe FeatureId
  -> Maybe Text
  -> Maybe Year
  -> Text
mapLegendLabel indicator featureId transform year = do
    let chartLabel = textSubstitution
                        Nothing
                        Nothing
                        (toMaybe indicator)
                        featureId
                        Nothing
                        year

    let label = case transform of
            Just tr -> textLabel indicator tr
            Nothing -> ""

    chartLabel label
