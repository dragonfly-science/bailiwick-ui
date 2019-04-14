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

import Control.Monad (void)

import Data.Text (Text)

import Reflex
import Reflex.Dom.Core
import Language.Javascript.JSaddle (jsg3, MonadJSM, liftJSM)

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
  -> m ()
mapLegend MapLegendState{..} = do
  readyE <- getPostBuild
  let initialUpdate = tagPromptlyDyn inputValuesD readyE
      width  = 481 :: Int
      height = 120 :: Int
  updateE <- switchHold initialUpdate (updated inputValuesD <$ readyE)
  performEvent_ $ ffor updateE $ \case
    Just d -> liftJSM . void $ do
        jsg3 ("updateMapLegend" :: Text) width height d
    _ -> return ()

