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
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Data.Maybe (isJust, listToMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict.InsOrd as OM
import Data.Aeson (Value)
import Language.Javascript.JSaddle (jsg3, MonadJSM, liftJSM)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex
import Reflex.Dom.Core hiding (Value)
import Reflex.Dom.Builder.Class (HasDomEvent(..))
import Reflex.Dom.Builder.Class.Events (EventName(..))
import Reflex.PostBuild.Class (PostBuild(..))

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
  let summaryD :: Dynamic t (Maybe AreaSummary)
      summaryD = flip OM.lookup summaries <$> (areaId <$> area)
      lookupIndicatorById i = OM.lookup (IndicatorId i) indicators
      ll :: IndicatorId -> Maybe AreaSummary -> Maybe ValueYear
      ll indid msummary = do
            summary <- msummary
            vals <- join $ OM.lookup indid summary
            listToMaybe vals
      lookupValueYear :: Text -> Dynamic t (Maybe ValueYear)
      lookupValueYear i = ll (IndicatorId i) <$> summaryD
  gotoIndicatorE <- leftmost <$> sequence
    [ indicatorLatestYearSummary
        "population"
        (lookupIndicatorById "population-estimates")
        "Population estimate"
        (lookupValueYear "population-estimates")
    , indicatorLatestYearSummary
        "income"
        (lookupIndicatorById "household-income-mean")
        "Average household income"
        (lookupValueYear "household-income-mean")
    , indicatorLatestYearSummary
        "housing"
        (lookupIndicatorById "mean-weekly-rent")
        "Mean weekly rent"
        (lookupValueYear "mean-weekly-rent")
    , indicatorLatestYearSummary
        "economic"
        (lookupIndicatorById "gdp-per-capita")
        "GDP per capita"
        (lookupValueYear "gdp-per-capita")
--      , indicatorSummary
--          "house-price"
--          (lookupIndicatorById "mean-house-value")
--          "Mean house value"
--          (housePriceTimeSeries areaD $ lookupValue "housePriceSeries")
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
  -> Dynamic t (Maybe ValueYear)   -- Year
  -> m (Event t Indicator)
indicatorLatestYearSummary cssClass indicator label valueyearD = do
  let labelyear (ValueYear _ y) = label <> " (" <> (T.pack $ show y) <> ")"
      numbers   (ValueYear v _) = T.pack $ show v
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
