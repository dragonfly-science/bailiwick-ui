{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Bailiwick.View.MapLegend
  ( mapLegend
  , MapLegendState(..)
  )
where

import Data.Text (Text)

import Reflex
import Reflex.Dom.Core
import Language.Javascript.JSaddle (jsg3, MonadJSM, liftJSM, valToObject)

import Bailiwick.Types

data MapLegendState t
  = MapLegendState
    { inputValuesD :: Dynamic t (Maybe [(Double, Maybe Display, Colour)])
    }

mapLegend
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => MapLegendState t
  -> m (Event t ScaleFunction)
mapLegend MapLegendState{..} = do
  readyE <- getPostBuild
  let initialUpdate = tagPromptlyDyn inputValuesD readyE
      width  = 481 :: Int
      height = 120 :: Int
  updateE <- switchHold initialUpdate (updated inputValuesD <$ readyE)
  performEvent $ ffor (fmapMaybeCheap id updateE) $ \d ->
    liftJSM $ do
      scaleVal <- jsg3 ("updateMapLegend" :: Text) width height d
      scale <- valToObject scaleVal
      return (ScaleFunction scale)

