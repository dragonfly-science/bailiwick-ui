-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}
module Bailiwick.View.IndicatorChart (
  indicatorChart
) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import Language.Javascript.JSaddle
       (jsg2, MonadJSM, liftJSM)
import Reflex.Dom.Core
import Reflex.PerformEvent.Class (PerformEvent(..))
import qualified GHCJS.DOM.Types as DOM (Element(..))

import Bailiwick.AreaTrees (AreaTrees)
import Bailiwick.State (getThemePage, ThemePageArgs(..), Message, State(..))
import Bailiwick.Types

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
  let _year = fmap themePageYear . getThemePage <$> state
      _iId = fmap themePageIndicatorId . getThemePage <$> state
      _areaTree =
        (getThemePage <$> state) >>=
            mapM (\ themePage ->
                let y = themePageYear themePage
                    ind = themePageIndicatorId themePage
                in return $ OM.lookup (IndicatorId $ unIndicatorId ind <> "-" <> T.pack (show y)) areaTrees)
      indicator = ((`OM.lookup` indicators) =<<) . fmap themePageIndicatorId . getThemePage <$> state

  -- display iId
  -- display year
  -- display areaTree

  display indicator

  -- text $ T.pack (show areaTrees)

  let _showAttr True  = "display: block"
      _showAttr False = "display: none"
  (e, _) <- elDynAttr' "div" (constDyn $ "class" =: "basic-barchart") $
      elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn $ "class"=:"d3-attach" <> "width"=:"480" <> "height"=:"530") $ return ()


  performEvent_ $ ffor postBuild $ \() -> do
    _ <- liftJSM $ jsg2 ("updateAreaBarchart" :: Text) (_element_raw e :: DOM.Element) ("text" :: Text)
    return ()

  return never
