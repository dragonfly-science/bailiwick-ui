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
import Data.Maybe (listToMaybe, isJust)

import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OM
import Language.Javascript.JSaddle (jsg3, MonadJSM, liftJSM)
import Reflex
import Reflex.Dom.Core

import Bailiwick.Types
import Bailiwick.Route

data AreaSummaryState t
  = AreaSummaryState
  { area       :: Dynamic t (Maybe Area)
  , summaries  :: Dynamic t AreaSummaries
  , indicators :: Dynamic t Indicators
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
  let lookupIndicatorById i = OM.lookup (IndicatorId i) <$> indicators
      lookupAreaSummary i   = OM.lookup (IndicatorId i) <$> summaries
      areaIdD = maybe "" areaId <$> area
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
        (return ()) -- (housePriceTimeSeries area $ lookupAreaSummary  "mean-house-value")
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
  => Text                        -- CSS class
  -> Dynamic t (Maybe Indicator) -- Indicator (for click message)
  -> Dynamic t Text              -- Label text
  -> m ()                        -- Content
  -> m (Event t Indicator)
indicatorSummary cssClass mbIndicatorD label content = do
  (e, _) <- elAttr' "div" ("class" =: ("summary-item " <> cssClass <> "-item")) $ do
      divClass "block-label" $ dynText label
      content
  return $ fmapMaybe id $ tagPromptlyDyn mbIndicatorD (domEvent Click e)

indicatorLatestYearSummary
  :: (Monad m, PostBuild t m, DomBuilder t m)
  => Text                          -- CSS class
  -> Dynamic t (Maybe Indicator)   -- Indicator (for click message)
  -> Text                          -- Label text
  -> Dynamic t AreaId              -- Which area ?
  -> Dynamic t (Maybe AreaSummary) -- Data
  -> m (Event t Indicator)
indicatorLatestYearSummary cssClass indicatorD label areaIdD summaryD = do
  let valueyearD = do
        areaid <- areaIdD
        msummary <- summaryD
        return $ do
          summary <- msummary
          myearvalues <- OM.lookup areaid summary
          yearvalues <- myearvalues
          listToMaybe yearvalues
      labelyear (YearValueDisp (y, _, _)) = label <> " (" <> y <> ")"
      numbers   (YearValueDisp (_, _, v)) = v
  indicatorSummary cssClass indicatorD (maybe "" labelyear <$> valueyearD) $
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
  => Dynamic t (Maybe Area)
  -> Dynamic t (Maybe AreaSummary)
  -> m ()
housePriceTimeSeries areaD msummaryD = do
  areaD' <- holdUniqDyn areaD
  let un = maybe [] (map unYearValueDisp)
      inputValues = do
        area <- areaD'
        msummary <- msummaryD
        let nzvals = join $ OM.lookup "new-zealand" =<< msummary
            areaid = maybe "" areaId area
            mvals = OM.lookup areaid =<< msummary
        case mvals of
          Nothing -> return Nothing
          Just vals -> do
            if areaid == "new-zealand"
              then return (Just ([(areaid, un vals)], area))
              else return (Just ([("new-zealand", un nzvals)
                                 ,(areaid, un vals)], area))
  (e, _) <- elAttr' "div" ("class" =: "houseprice-timeseries") $ do
    elAttr "div" (  "class" =: "d3-attach"
                 <> "style" =: "width: 225px; height: 120px") $ return ()
    divClass "time-series-legend" $ do
      el "span" $ dynText $ (("— " <>) . maybe "" areaName) <$> areaD'
      el "span" $ dynText $ (\a -> if fmap areaId a == Just "new-zealand"
                                     then ""
                                     else " — New Zealand") <$> areaD'

  initialUpdate <- tagPromptlyDyn inputValues <$> (delay 0.5 =<< getPostBuild)
  let updateE = ffilter isJust $ leftmost [updated inputValues, initialUpdate]
  performEvent_ $ ffor updateE $ \case
    Just (d, area) -> liftJSM . void $ do
         jsg3 ("updateTimeSeries" :: Text) (_element_raw e) d (maybe "" areaId area)
    _ -> return ()


