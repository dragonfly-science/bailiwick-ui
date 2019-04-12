{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase          #-}
module Bailiwick.View.MapLegend
  ( mapLegend
  )
where

import Control.Monad (void)

import Data.Text (Text)

import Reflex
import Reflex.Dom.Core
import Language.Javascript.JSaddle (jsg2, MonadJSM, liftJSM)

import Bailiwick.Types

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
  => Dynamic t (Maybe [(Double, Text, Text)])
  -> m ()
mapLegend inputValuesD = do
  (e, _) <- elAttr' "div" (  "class" =: "d3-attach"
                          <> "style" =: "width: 481px; height: 120px") $ return ()
  readyE <- getPostBuild
  let initialUpdate = tagPromptlyDyn inputValuesD readyE
  let updateValuesE = updated inputValuesD
  updateE <- switchHold initialUpdate (updateValuesE <$ readyE)
  performEvent_ $ ffor updateE $ \case
    Just (d) -> liftJSM . void $ do
         jsg2 ("updateMapLegend" :: Text) (_element_raw e) d
    _ -> return ()

