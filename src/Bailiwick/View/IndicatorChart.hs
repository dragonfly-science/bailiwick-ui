-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.IndicatorChart (
  indicatorChart
) where

import Control.Monad (void)
import Data.Maybe (isJust, maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Javascript.JSaddle
       (jsg2, MonadJSM, liftJSM, new)
import Reflex.Dom.Core
       (display, elAttr', elDynAttr', elDynAttrNS, elDynAttrNS',
        GhcjsDomSpace, DomBuilderSpace, el, display, dynText, DomBuilder, elAttr,
        text, (=:), elClass, divClass, Dynamic, _element_raw, Event, PostBuild, never, getPostBuild)
import Reflex (Dynamic(..), TriggerEvent, constDyn, ffor)
import Reflex.PerformEvent.Class (PerformEvent(..))
import GHCJS.DOM.Types (Element(..), HTMLElement(..))

import Bailiwick.AreaTrees (AreaTrees, AreaTree)
import Bailiwick.State
       (getThemePage, ThemePageArgs(..), Message, State(..))
import Bailiwick.Types (Indicators)

import qualified Data.HashMap.Strict.InsOrd as OM (lookup, toList, elems, keys)

indicatorChart
  :: ( Monad m
        , PostBuild t m
        , DomBuilder t m
        , PerformEvent t m
        , TriggerEvent t m
        , MonadJSM (Performable m)
        , DomBuilderSpace m ~ GhcjsDomSpace
        )
  => AreaTrees
  -> Indicators
  -> Dynamic t State
  -> m (Event t Message)
indicatorChart areaTrees indicators state = do
  postBuild <- getPostBuild
  let year = fmap themePageYear . getThemePage <$> state
      indicatorId = fmap themePageIndicatorId . getThemePage <$> state
      -- areaTree = OM.lookup areaTrees (indicatorId <> "-" <> (show year))
      indicator = ((`OM.lookup` indicators) =<<) . fmap themePageIndicatorId . getThemePage <$> state
  
  display indicatorId
  display year
  -- display areaTree
  display indicator
  
  let showAttr True  = "display: block"
      showAttr False = "display: none"
  (e, _) <- elDynAttr' "div" (constDyn $ "class" =: "basic-barchart") $ do
      elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn $ "class"=:"d3-attach" <> "width"=:"480" <> "height"=:"530") $ return ()
  
    
  performEvent_ $ ffor postBuild $ \() -> do
    liftJSM $ jsg2 ("updateAreaBarchart" :: Text) (_element_raw e :: Element) ("text" :: Text)
    return ()
  
  return never
