{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.IndicatorSummary (
    indicatorSummary
  , IndicatorSummaryState(..)
) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)
import Data.Text (Text)

import Reflex.Dom.Core hiding (elDynHtmlAttr')
import Language.Javascript.JSaddle (MonadJSM)

import Bailiwick.View.Text
import Bailiwick.Javascript (elDynHtmlAttr')
import Bailiwick.Types

data IndicatorSummaryState t
  = IndicatorSummaryState
  { areaD              :: Dynamic t (Loadable Area)
  , compareAreaD       :: Dynamic t (Loadable Area)
  , featureD           :: Dynamic t (Maybe FeatureId)
  , yearD              :: Dynamic t (Maybe Year)
  , indicatorD         :: Dynamic t (Loadable Indicator)
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
        lareaid <- fmap areaId <$> areaD
        lyear   <- toLoadable <$> yearD
        feature <- featureD
        IndicatorNumbers ismap <- indicatorNumbersD
        return $ do  -- Loadable
          areaid <- lareaid
          year <- lyear
          toLoadable $ OM.lookup (areaid, year, feature) ismap

      compareNumsD = do
        lareaid <- fmap areaId <$> compareAreaD
        lyear   <- toLoadable <$> yearD
        feature <- featureD
        IndicatorNumbers ismap <- indicatorNumbersD
        return $ do
          areaid <- lareaid
          year <- lyear
          toLoadable $ OM.lookup (areaid, year, feature) ismap

      notesD = do
        lindicator <- indicatorD
        case indicatorNotes <$> lindicator of
           Loaded (Just notes) -> return notes
           _                   -> return []

      showNumber :: Loadable Text -> Text
      showNumber = \case
        Loaded x -> x
        Loading  -> "..."
        Missing  -> "No data"


  divClass "summary" $
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        divClass "number" $
          dynText (showNumber . fmap headlineDisp <$> summaryNumsD)
        divClass "comparison-number hidden" $
          dynText (showNumber . fmap headlineDisp <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ load "" indicatorHeadlineNumCaption <$> indicatorD
      divClass "intersection-number regional-value" $ do
        divClass "number" $
          dynText (showNumber . fmap localDisp <$> summaryNumsD)
        divClass "comparison-number" $
          dynText (showNumber . fmap localDisp <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ load "" indicatorLocalNumCaption <$> indicatorD
      divClass "intersection-number national-value" $ do
        divClass "number" $
          dynText (showNumber . fmap nationalDisp <$> summaryNumsD)
        divClass "comparison-number" $
          dynText (showNumber . fmap nationalDisp <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ load "" indicatorNationalNumCaption <$> indicatorD
  divClass "summary-links" $ do
    void . elDynHtmlAttr' "div" (constDyn $ "class" =: "source") $
           ("Source: " <>) <$> subs (load "" indicatorPublishers <$> indicatorD)
    divClass "notes" $
      el "div" $ do
        text "Notes:"
        void . dyn $ ffor notesD
                   (mapM_ $ elDynHtmlAttr' "p" (constDyn mempty) . subs . constDyn)





