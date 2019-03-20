{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.AreaSummary (
  areaSummary
) where

import Control.Monad.Fix (MonadFix)
import Control.Monad (void)
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)

import qualified Data.HashMap.Strict.InsOrd as OM (lookup)
import Data.Aeson ((.:), Object, FromJSON, Value)
import Data.Aeson.Types (parseMaybe)
import Language.Javascript.JSaddle (jsg3, MonadJSM, liftJSM)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex (TriggerEvent, delay, leftmost, tagPromptlyDyn,
       ffor, holdUniqDyn, PostBuild, updated, fmapMaybe)
import Reflex.Dom.Core
       (elAttr', elDynAttr',
        GhcjsDomSpace, DomBuilderSpace, MonadHold, el, dynText, DomBuilder, elAttr,
        text, (=:), divClass, Dynamic, _element_raw, Event, never)
import Reflex.Dom.Builder.Class (HasDomEvent(..))
import Reflex.Dom.Builder.Class.Events (EventName(..))
import Reflex.PostBuild.Class (PostBuild(..))

import Bailiwick.State (State)
import qualified Bailiwick.State as State
import Bailiwick.Types
import Bailiwick.Route
       (ThemePageArgs(..), Message(..), Message, Page(..))

indicatorSummary
  :: (Monad m, PostBuild t m, DomBuilder t m)
  => Text
  -> Dynamic t (Maybe Indicator)
  -> Dynamic t Text
  -> m ()
  -> m (Event t Indicator)
indicatorSummary cssClass mbIndicator label content = do
  (e, _) <- elAttr' "div" ("class" =: ("summary-item " <> cssClass <> "-item")) $ do
      divClass "block-label" $ dynText label
      content
  return $ fmapMaybe id $ tagPromptlyDyn mbIndicator (domEvent Click e)

indicatorLatestYearSummary
  :: (Monad m, PostBuild t m, DomBuilder t m)
  => Text
  -> Dynamic t (Maybe Indicator)
  -> Text
  -> Dynamic t (Maybe Text)
  -> m ()
  -> m (Event t Indicator)
indicatorLatestYearSummary cssClass indicator label year numbers =
  indicatorSummary cssClass indicator (maybe "" (\y -> label <> " (" <> y <> ")") <$> year) $
    divClass ("summary-numbers " <> cssClass <> "-numbers") $ do
      el "i" $ return ()
      numbers

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
  => Dynamic t State
  -> m (Event t Message)
areaSummary stateD = do
  let areaIdD = State.stateArea <$> stateD
      areaD :: Dynamic t (Maybe Area)
      areaD = OM.lookup <$> areaIdD <*> (unAreas . State.getAreas <$> stateD)
      summaryD :: Dynamic t (Maybe AreaSummary)
      summaryD = OM.lookup <$> areaIdD <*> (State.getAreaSummaries <$> stateD)
      indicatorValuesD :: Dynamic t (Maybe Object)
      indicatorValuesD = fmap areaSummaryIndicatorValues <$> summaryD
      convertValue p = parseMaybe (const p) ()
      lookupValue :: forall a. FromJSON a => Text -> Dynamic t (Maybe a)
      lookupValue n = ((convertValue . (.: n)) =<<) <$> indicatorValuesD
      lookupIndicatorById i = OM.lookup (IndicatorId i) <$> (State.getIndicators <$> stateD)
      textValue n = dynText $ fromMaybe "" <$> lookupValue n
  gotoIndicatorE <- leftmost <$> sequence
    [ indicatorLatestYearSummary
        "population"
        (lookupIndicatorById "population-estimates")
        "Population estimate"
        (lookupValue "populationEstimateYear")
        (textValue "populationEstimate")
    , indicatorLatestYearSummary
        "income"
        (lookupIndicatorById "average-household-income")
        "Average household income"
        (lookupValue "averageHouseholdIncomeYear")
        (textValue "averageHouseholdIncome")
    , indicatorLatestYearSummary
        "housing"
        (lookupIndicatorById "mean-weekly-rent")
        "Mean weekly rent"
        (lookupValue "meanWeeklyRentYear")
        (textValue "meanWeeklyRent")
    , indicatorLatestYearSummary
        "economic"
        (lookupIndicatorById "gdp-per-capita")
        "GDP per capita"
        (lookupValue "gdpPerCapitaYear")
        (textValue "gdpPerCapita")
    , indicatorSummary
        "house-price"
        (lookupIndicatorById "mean-house-value")
        "Mean house value"
        (housePriceTimeSeries areaD $ lookupValue "housePriceSeries")
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
  -> Dynamic t (Maybe Value)
  -> m ()
housePriceTimeSeries areaD dataD = do
  dataD' <- holdUniqDyn dataD
  areaD' <- holdUniqDyn areaD
  let inputValues = liftA2 (,) <$> dataD' <*> areaD'
      showAttr True  = mempty
      showAttr False = "style" =: "display: none"
  (e, _) <- elDynAttr' "div" (("class" =: "houseprice-timeseries" <>) . showAttr . isJust <$> inputValues) $ do
    elAttr "div" ("class"=:"d3-attach" <> "style"=:"width: 225px; height: 120px") $ return ()
    divClass "time-series-legend" $ do
      el "span" $ dynText $ maybe "" (("— " <>) . areaName) <$> areaD
      el "span" $ dynText $ maybe "" (\a -> if areaId a == "new-zealand" then "" else " — New Zealand") <$> areaD
  initialUpdate <- tagPromptlyDyn inputValues <$> (delay 0.5 =<< getPostBuild)
  performEvent_ $ ffor (leftmost [updated inputValues, initialUpdate]) $ \case
    Just (d, area) -> liftJSM . void $ jsg3 ("updateTimeSeries" :: Text) (_element_raw e) d (areaName area)
    _ -> return ()
