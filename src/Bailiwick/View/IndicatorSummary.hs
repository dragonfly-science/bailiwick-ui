{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bailiwick.View.IndicatorSummary (indicatorSummary) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)
import Data.Text (Text)

import Reflex.Dom.Core hiding (elDynHtmlAttr')
import Language.Javascript.JSaddle (MonadJSM)

import Bailiwick.View.Text
import Bailiwick.Javascript (elDynHtmlAttr')
import Bailiwick.State
       (State(State, selectedAreaD, compareAreaD, featureD, yearD,
              indicatorD, indicatorNumbersD))
import Bailiwick.Types

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
  => State t
  -> m ()
indicatorSummary
  State{selectedAreaD,compareAreaD,featureD,yearD,indicatorD,indicatorNumbersD} = do

  let subs = (textSubstitution
                <$> (toMaybe <$> selectedAreaD)
                <*> (load Nothing id <$> compareAreaD)
                <*> (toMaybe <$> indicatorD)
                <*> featureD
                <*> (constDyn Nothing)
                <*> yearD
                <*>)

      summaryNumsD = do
        lareaid <- fmap areaId <$> selectedAreaD
        lyear   <- toLoadable <$> yearD
        feature <- featureD
        lindnumbers <- indicatorNumbersD
        return $ do  -- Loadable
          areaid <- lareaid
          year <- lyear
          IndicatorNumbers ismap <- lindnumbers
          toLoadable $ OM.lookup (areaid, year, feature) ismap

      compareNumsD = do -- Dynamic t
        lmareaid <- fmap (fmap areaId) <$> compareAreaD
        lyear   <- toLoadable <$> yearD
        feature <- featureD
        lindnumbers <- indicatorNumbersD
        return $ do -- Loadable
          mareaid <- lmareaid
          year <- lyear
          IndicatorNumbers ismap <- lindnumbers
          return $ do -- Maybe
            areaid <- mareaid
            OM.lookup (areaid, year, feature) ismap

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

      showNumberComp :: Loadable (Maybe Text) -> Text
      showNumberComp = \case
        Loaded (Just x) -> x
        Loaded Nothing -> ""
        Loading  -> "..."
        Missing  -> "No data"


  divClass "summary" $
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        divClass "number" $
          dynText (showNumber . fmap headlineDisp <$> summaryNumsD)
        divClass "comparison-number" $
          dynText (showNumberComp . fmap (fmap headlineDisp) <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ load "" indicatorHeadlineNumCaption <$> indicatorD
      divClass "intersection-number regional-value" $ do
        divClass "number" $
          dynText (showNumber . fmap localDisp <$> summaryNumsD)
        divClass "comparison-number" $
          dynText (showNumberComp . fmap (fmap localDisp) <$> compareNumsD)
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ load "" indicatorLocalNumCaption <$> indicatorD
      divClass "intersection-number national-value" $ do
        divClass "number" $
          dynText (showNumber . fmap nationalDisp <$> summaryNumsD)
        divClass "comparison-number" $
          dynText (showNumberComp . fmap (fmap nationalDisp) <$> compareNumsD)
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





