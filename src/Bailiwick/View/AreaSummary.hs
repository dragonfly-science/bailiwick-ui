{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.AreaSummary (
    areaSummary
  , AreaSummaryState(..)
) where

import Control.Monad.Fix (MonadFix)
import Control.Monad (void, join)
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict.InsOrd as OM
import Language.Javascript.JSaddle (jsg3, MonadJSM, liftJSM)
import Reflex
import Reflex.Dom.Core

import Bailiwick.Types
import Bailiwick.Route

data AreaSummaryState t
  = AreaSummaryState
  { area       :: Dynamic t Area
  , summaries  :: AreaSummaries
  , indicators :: Indicators
  }

areaSummary
  :: forall m t.
     ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , MonadHold t m
     , MonadFix m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => AreaSummaryState t
  -> m (Event t Message)
areaSummary AreaSummaryState{..} = do
  let lookupIndicatorById i = OM.lookup (IndicatorId i) indicators
      lookupAreaSummary i   = OM.lookup (IndicatorId i) summaries
      areaIdD = areaId <$> area
  gotoIndicatorE <- leftmost <$> sequence
    [ indicatorLatestYearSummary
        "population"
        (lookupIndicatorById "population-estimates")
        "Population estimate"
        areaIdD
        (lookupAreaSummary "population-estimates")
    , indicatorLatestYearSummary
        "income"
        (lookupIndicatorById "household-income-mean")
        "Average household income"
        areaIdD
        (lookupAreaSummary "household-income-mean")
    , indicatorLatestYearSummary
        "housing"
        (lookupIndicatorById "mean-weekly-rent")
        "Mean weekly rent"
        areaIdD
        (lookupAreaSummary "mean-weekly-rent")
    , indicatorLatestYearSummary
        "economic"
        (lookupIndicatorById "gdp-per-capita")
        "GDP per capita"
        areaIdD
        (lookupAreaSummary "gdp-per-capita")
    , indicatorSummary
        "house-price"
        (lookupIndicatorById "mean-house-value")
        "Mean house value"
        (housePriceTimeSeries area $ lookupAreaSummary  "mean-house-value")
    , divClass "summary-item button" $
        elAttr "a" ("class" =: "indicators right" <> "href" =: "#indicators") $
          el "span" $ do
            text "All indicators"
            el "i" $ return ()
            return never
    ]
  return $ (\Indicator{..} ->
      GoTo (ThemePage $ ThemePageArgs
        indicatorId
        indicatorDefaultChartLeft
        indicatorDefaultChartRight
        2017 Nothing Nothing "reg" "indexed" "indexed")
        ) <$> gotoIndicatorE

-- Display indicator summary block
indicatorSummary
  :: (Monad m, PostBuild t m, DomBuilder t m)
  => Text                     -- CSS class
  -> Maybe Indicator          -- Indicator (for click message)
  -> Dynamic t Text           -- Label text
  -> m ()                     -- Content
  -> m (Event t Indicator)
indicatorSummary cssClass mbIndicator label content = do
  (e, _) <- elAttr' "div" ("class" =: ("summary-item " <> cssClass <> "-item")) $ do
      divClass "block-label" $ dynText label
      content
  return $ fmapMaybe id $ (mbIndicator <$ domEvent Click e)

indicatorLatestYearSummary
  :: (Monad m, PostBuild t m, DomBuilder t m)
  => Text                          -- CSS class
  -> Maybe Indicator               -- Indicator (for click message)
  -> Text                          -- Label text
  -> Dynamic t AreaId              -- Which area ?
  -> Maybe AreaSummary             -- Data
  -> m (Event t Indicator)
indicatorLatestYearSummary _ _ _ _ Nothing =do return never
indicatorLatestYearSummary cssClass indicator label areaIdD (Just summary) = do
  let valueyearD = do
        areaid <- areaIdD
        return $ do
          myearvalues <- OM.lookup areaid summary
          yearvalues <- myearvalues
          listToMaybe yearvalues
      labelyear (YearValue (y, _)) = label <> " (" <> (T.pack $ show y) <> ")"
      numbers   (YearValue (_, v)) = T.pack $ show v
  indicatorSummary cssClass indicator (maybe "" labelyear <$> valueyearD) $
    divClass ("summary-numbers " <> cssClass <> "-numbers") $ do
      el "i" $ return ()
      dynText (maybe "" numbers <$> valueyearD)

housePriceTimeSeries
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , MonadHold t m
     , MonadFix m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Dynamic t Area
  -> Maybe AreaSummary
  -> m ()
housePriceTimeSeries _ Nothing = return ()
housePriceTimeSeries areaD (Just summary) = do
  areaD' <- holdUniqDyn areaD
  let nzvals = join $ OM.lookup "new-zealand" summary
      un = maybe [] (map unYearValue)
      inputValues = do
        area <- areaD'
        let areaid = areaId area
            mvals = OM.lookup areaid summary
        case mvals of
          Nothing -> return (Just ([], area))
          Just vals -> do
            if areaid == "new-zealand"
              then return (Just ([(areaid, un vals)], area))
              else return (Just ([("new-zealand", un nzvals)
                                 ,(areaid, un vals)], area))
  (e, _) <- elAttr' "div" ("class" =: "houseprice-timeseries") $ do
    elAttr "div" (  "class" =: "d3-attach"
                 <> "style" =: "width: 225px; height: 120px") $ return ()
    divClass "time-series-legend" $ do
      el "span" $ dynText $ (("— " <>) . areaName) <$> areaD'
      el "span" $ dynText $ (\a -> if areaId a == "new-zealand"
                                     then ""
                                     else " — New Zealand") <$> areaD'
  initialUpdate <- tagPromptlyDyn inputValues <$> (delay 0.5 =<< getPostBuild)
  performEvent_ $ ffor (leftmost [updated inputValues, initialUpdate]) $ \case
    Just (d, area) -> liftJSM . void $ do
         jsg3 ("updateTimeSeries" :: Text) (_element_raw e) d (areaName area)
    _ -> return ()


