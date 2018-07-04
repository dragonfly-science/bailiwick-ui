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

import Data.Text (Text)
import Reflex
       (TriggerEvent, delay, leftmost, tagPromptlyDyn, constDyn, ffor,
        PostBuild, updated)
import Reflex.Dom.Core
       (elDynAttr', elAttr', elDynAttrNS, elDynAttrNS', GhcjsDomSpace,
        DomBuilderSpace, el', el, dynText, DomBuilder, elAttr, text, (=:),
        elClass, divClass, Dynamic, _element_raw)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Text as T (pack)

import Bailiwick.Types
       (Areas, AreaSummary(..), Area(..), IndicatorID(..),
        AreaSummaries(..))
import Bailiwick.State (getArea, State(..))
import qualified Data.Map.Ordered as OM (lookup)
import Control.Monad (void, join)
import qualified Data.HashMap.Strict as HM (lookup)
import Data.Aeson ((.:), Object, (.:?), FromJSON, Value)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (isJust, fromMaybe)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Language.Javascript.JSaddle
       (jsg3, MonadJSM, liftJSM, jsNull, jsg2)
import Reflex.PostBuild.Class (PostBuild(..))
import Control.Applicative (liftA2)

indicatorSummary
  :: (Monad m, PostBuild t m, DomBuilder t m)
  => Text
  -> IndicatorID
  -> Dynamic t Text
  -> m ()
  -> m ()
indicatorSummary cssClass indicatorId label content =
  divClass ("summary-item " <> cssClass <> "-item") $ do
      divClass "block-label" $ dynText label
      content

indicatorLatestYearSummary
  :: (Monad m, PostBuild t m, DomBuilder t m)
  => Text
  -> IndicatorID
  -> Text
  -> Dynamic t (Maybe Text)
  -> m ()
  -> m ()
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
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Areas
  -> AreaSummaries
  -> Dynamic t State
  -> m ()
areaSummary areas areaSummaries state = do
  let areaIdD = maybe "new-zealand" areaId . getArea <$> state
      areaD :: Dynamic t (Maybe Area)
      areaD = (`OM.lookup` areas) <$> areaIdD
      summaryD :: Dynamic t (Maybe AreaSummary)
      summaryD = (`OM.lookup` areaSummaries) <$> areaIdD
      indicatorsD :: Dynamic t (Maybe Object)
      indicatorsD = fmap areaSummaryIndicatorValues <$> summaryD
      convertValue p = parseMaybe (const p) ()
      lookupValue :: forall a. FromJSON a => Text -> Dynamic t (Maybe a)
      lookupValue n = ((convertValue . (.: n)) =<<) <$> indicatorsD
      textValue n = dynText $ fromMaybe "" <$> lookupValue n
  indicatorLatestYearSummary
    "population"
    "population-estimates"
    "Population estimate"
    (lookupValue "populationEstimateYear")
    (textValue "populationEstimate")
  indicatorLatestYearSummary
    "income"
    "household-income"
    "Average household income"
    (lookupValue "averageHouseholdIncomeYear")
    (textValue "averageHouseholdIncome")
  indicatorLatestYearSummary
    "housing"
    "mean-weekly-rent"
    "Mean weekly rent"
    (lookupValue "meanWeeklyRentYear")
    (textValue "meanWeeklyRent")
  indicatorLatestYearSummary
    "economic"
    "gdp-per-capita"
    "GDP per capita"
    (lookupValue "gdpPerCapitaYear")
    (textValue "gdpPerCapita")
  indicatorSummary
    "house-price"
    "mean-house-value"
    "Mean house value"
    (housePriceTimeSeries areaD $ lookupValue "housePriceSeries")
  divClass "summary-item button" $
    elAttr "a" ("class" =: "indicators right" <> "href" =: "#indicators") $
      el "span" $ do
        text "All indicators"
        el "i" $ return ()

housePriceTimeSeries
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Dynamic t (Maybe Area)
  -> Dynamic t (Maybe Value)
  -> m ()
housePriceTimeSeries areaD dataD = do
  let inputValues = liftA2 (,) <$> dataD <*> areaD
      showAttr True  = mempty
      showAttr False = "style" =: "display: none"
  (e, _) <- elDynAttr' "div" (("class" =: "houseprice-timeseries" <>) . showAttr . isJust <$> inputValues) $ do
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn $ "class"=:"d3-attach" <> "width"=:"225" <> "height"=:"120") $ return ()
    divClass "time-series-legend" $ do
      el "span" $ dynText $ maybe "" (("— " <>) . areaName) <$> areaD
      el "span" $ dynText $ maybe "" (\a -> if areaId a == "new-zealand" then "" else " — New Zealand") <$> areaD
  initialUpdate <- tagPromptlyDyn inputValues <$> (delay 0.5 =<< getPostBuild)
  performEvent_ $ ffor (leftmost [updated inputValues, initialUpdate]) $ \case
    Just (d, area) -> liftJSM . void $ jsg3 ("updateTimeSeries" :: Text) (_element_raw e) d (areaName area)
    _ -> return ()
