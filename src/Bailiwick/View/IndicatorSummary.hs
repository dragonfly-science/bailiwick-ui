{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.IndicatorSummary (
    indicatorSummary
  , IndicatorSummaryState(..)
) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import Reflex.Dom.Core hiding (elDynHtmlAttr')
import Language.Javascript.JSaddle (MonadJSM)

import Bailiwick.View.Text
import Bailiwick.Javascript (elDynHtmlAttr')
import Bailiwick.Types

data IndicatorSummaryState t
  = IndicatorSummaryState
  { areaD              :: Dynamic t (Maybe Area)
  , compareAreaD       :: Dynamic t (Maybe Area)
  , featureD           :: Dynamic t (Maybe FeatureId)
  , yearD              :: Dynamic t (Maybe Year)
  , indicatorD         :: Dynamic t (Maybe Indicator)
  , indicatorNumbersD  :: Dynamic t IndicatorNumbers
  }

indicatorSummary
  :: forall m t.
     ( Monad m
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => IndicatorSummaryState t
  -> m ()
indicatorSummary IndicatorSummaryState{..} = do

  let subs = (textSubstitution
                <$> areaD
                <*> compareAreaD
                <*> indicatorD
                <*> featureD
                <*> (constDyn Nothing)
                <*> yearD
                <*>)
      summaryNumsD = do
        mareaid <- fmap areaId <$> areaD
        myear   <- yearD
        feature <- featureD
        IndicatorNumbers ismap <- indicatorNumbersD
        return $ fromMaybe emptyNumbers $ do
          areaid <- mareaid
          year <- myear
          OM.lookup (areaid, year, feature) ismap

      compareNumsD = do
        mareaid <- fmap areaId <$> compareAreaD
        myear   <- yearD
        feature <- featureD
        IndicatorNumbers ismap <- indicatorNumbersD
        return $ fromMaybe emptyNumbers $ do
          areaid <- mareaid
          year <- myear
          OM.lookup (areaid, year, feature) ismap

  divClass "summary" $
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        divClass "number" $ dynText (headlineDisp <$> summaryNumsD)
        divClass "comparison-number hidden" $ dynText (headlineDisp <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorHeadlineNumCaption <$> indicatorD
      divClass "intersection-number regional-value" $ do
        divClass "number" $ dynText (localDisp <$> summaryNumsD)
        divClass "comparison-number" $ dynText (localDisp <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorLocalNumCaption <$> indicatorD
      divClass "intersection-number national-value" $ do
        divClass "number" $ dynText (nationalDisp <$> summaryNumsD)
        divClass "comparison-number" $ dynText (nationalDisp <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorNationalNumCaption <$> indicatorD
  divClass "summary-links" $ do
    void . elDynHtmlAttr' "div" (constDyn $ "class" =: "source") $
           ("Source: " <>) <$> subs (maybe "" indicatorPublishers <$> indicatorD)
    divClass "notes" $
      el "div" $ do
        text "Notes:"
        void . dyn $ ffor (fromMaybe [] . (indicatorNotes =<<) <$> indicatorD)
                       (mapM_ $ elDynHtmlAttr' "p" (constDyn mempty) . subs . constDyn)





