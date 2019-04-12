{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Bailiwick.View.IndicatorSummary (
    indicatorSummary
  , IndicatorSummaryState(..)
) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import Reflex
import Reflex.Dom.Core hiding (elDynHtmlAttr')
import GHCJS.DOM.Types (liftJSM, MonadJSM)
import GHCJS.DOM.Element (setInnerHTML)

import Bailiwick.View.Text
import Bailiwick.Types
import Bailiwick.Route

elDynHtmlAttr'
  :: ( Monad m
     , DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Text
  -> Dynamic t (Map Text Text)
  -> Dynamic t Text
  -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtmlAttr' elementTag attrs html = do
  (e, _) <- elDynAttr' elementTag attrs $ return ()
  postBuild <- getPostBuild
  performEvent_ $ liftJSM . setInnerHTML (_element_raw e) <$> leftmost [updated html, tag (current html) postBuild]
  return e

data IndicatorSummaryState t
  = IndicatorSummaryState
  { routeD             :: Dynamic t Route
  , areaD              :: Dynamic t (Maybe Area)
  , compareAreaD       :: Dynamic t (Maybe Area)
  , featureD           :: Dynamic t (Maybe Feature)
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
  -> m (Event t Message)
indicatorSummary IndicatorSummaryState{..} = mdo

  let subs = (textSubstitution
                <$> areaD
                <*> compareAreaD
                <*> indicatorD
                <*> featureD
                <*> (getThemePage <$> routeD)
                <*>)
      summaryNumsD = do
        mareaid <- fmap areaId <$> areaD
        myear   <- fmap themePageYear . getThemePage <$> routeD
        IndicatorNumbers ismap <- indicatorNumbersD
        return $ fromMaybe (Numbers ["", "", "", ""]) $ do
          areaid <- mareaid
          year <- myear
          OM.lookup (areaid, year, Nothing) ismap

  divClass "summary" $
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        divClass "number" $ dynText (headlineNum <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorHeadlineNumCaption <$> indicatorD
      divClass "intersection-number regional-value" $ do
        divClass "number" $ dynText (localNum <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorLocalNumCaption <$> indicatorD
      divClass "intersection-number national-value" $ do
        divClass "number" $ dynText (nationalNum <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
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
  elDynClass "div" (("table-view " <>) . bool "hide" "show" <$> showTableD) $
    elAttr "div" ("class" =: "panel" <> "style" =: "height: 799px;") $ do
      el "header" $
        divClass "table-caption text" $
          text "TODO"
      elAttr "div" ("class" =: "table-container" <> "style" =: "height: 657px;") $
        el "div" $
          text "TODO"

  showTableD <- holdDyn False $ not <$> tag (current showTableD) showTableE
  showTableE <- fmap (domEvent Click . fst) $
    divClass "table-button" $
      elClass' "button" "show-table" $
        el "span" $ do
          el "i" $ return ()
          dynText (bool "show table" "hide table" <$> showTableD)

  return never



