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
        return $ fromMaybe emptyNumbers $ do
          areaid <- mareaid
          year <- myear
          OM.lookup (areaid, year, Nothing) ismap

  showTableD <- holdDyn False $ not <$> tag (current showTableD) showTableE

  divClass "summary" $
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        divClass "number" $ dynText (headlineDisp <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorHeadlineNumCaption <$> indicatorD
      divClass "intersection-number regional-value" $ do
        divClass "number" $ dynText (localDisp <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorLocalNumCaption <$> indicatorD
      divClass "intersection-number national-value" $ do
        divClass "number" $ dynText (nationalDisp <$> summaryNumsD)
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
      el "header" $ do
        divClass "table-caption text" $
          text "TODO"
        divClass "controls" $ do
          -- requires an event to close the table.
          elAttr "button" ("class" =: "close") $
            el "i" $ return ()
     
      elAttr "div" ("class" =: "table-container" <> "style" =: "height: 657px;") $
        -- ##
        -- There are 2 tables - 1 with a single region/indicator,
        -- the second with comparison data.
        -- ##

        divClass "single-table" $ do
          -- may need logic to determine when to use "show" on the table class.
          elAttr "table" ("class" =: "table-sorter show") $ do
            el "tfoot" $
              el "tr" $
                elAttr "td" ("class" =: "button" <> "colspan" =: "4") $ do
                  -- there needs to be logic to check whether there can be
                  -- an export button
                  elAttr "button" ("class" =: "export") $ text "Export CSV"
            el "thead" $
              el "tr" $ do
                -- Each th element needs an event to add "tablesorter-headerDesc" or
                -- "tablesorter-headerAsc" if the column is being sorted.
                elAttr "th" ("class" =: "year tablesorter-headerDesc") $ text "Year"
                -- The next 3 header rows need to have correctly formatted titles
                -- - e.g. "The estimated resident population", "The annual
                -- percentage chage in ..."
                elAttr "th" ("class" =: "original") $ text "Original"
                elAttr "th" ("class" =: "transform") $ text "Transform"
                elAttr "th" ("class" =: "national") $ text "National"
            el "tbody" $ 
              -- each row represents 1 row in the data set
              el "tr" $ do
                el "td" $ text "Row Year"
                elAttr "td" ("class" =: "colour-teal") $ text "Row Original"
                elAttr "td" ("class" =: "colour-lighter-blue") $ text "Row Transform"
                elAttr "td" ("class" =: "colour-green") $ text "Row National"

        -- ##
        -- If the comparision option is enabled, we show the comparison data...
        -- ##
        -- divClass "compare-table" $ do
        --   -- may need logic to determine when to use "show" on the table class.
        --   elAttr "table" ("class" =: "table-sorter show") $ do
        --     el "tfoot" $
        --       el "tr" $ do
        --         elAttr "td" ("class" =: "button") $ do
        --           -- there needs to be logic to check whether there can be
        --           -- an export button
        --           elAttr "button" ("class" =: "export") $ text "Export CSV"
        --         elAttr "td" ("colspan" =: "3") $ do
        --           el "span" $ text "The estimated resident population -- original label"
        --         elAttr "td" ("colspan" =: "2") $ do
        --           el "span" $ text "The annual percentage change in the estimated resident population -- transform label"
        --     el "thead" $
        --       el "tr" $ do
        --         -- Each th element needs an event to add "tablesorter-headerDesc" or
        --         -- "tablesorter-headerAsc" if the column is being sorted.
        --         elAttr "th" ("class" =: "double tablesorter-headerDesc") $ text "Year"
        --         -- The next header rows need to have correctly formatted titles
        --         -- - e.g. "The estimated resident population", "The annual
        --         -- percentage chage in ..."
        --         elAttr "th" ("class" =: "border-top area") $ text "Area Name"
        --         elAttr "th" ("class" =: "border-top compare") $ text "Compare Area Name"
        --         elAttr "th" ("class" =: "border-top double ratio") $ text "Ratio"
        --         elAttr "th" ("class" =: "border-top area-t") $ text "Area Name"
        --         elAttr "th" ("class" =: "border-top compare-t") $ text "Compare Area Name"
        --     el "tbody" $ 
        --       -- each row represents 1 row in the data set
        --       el "tr" $ do
        --         elAttr "td" ("class" =: "double") $ text "Row Year"
        --         elAttr "td" ("class" =: "colour-green") $ text "Row Area"
        --         elAttr "td" ("class" =: "colour-compare-green") $ text "Row Compare Area"
        --         elAttr "td" ("class" =: "double") $ text "Row Ratio"
        --         elAttr "td" ("class" =: "colour-green") $ text "Row Area-t"
        --         elAttr "td" ("class" =: "colour-compare-green") $ text "Row Compare-t"

  
  showTableE <- fmap (domEvent Click . fst) $
    divClass "table-button" $
      elClass' "button" "show-table" $
        el "span" $ do
          el "i" $ return ()
          dynText (bool "show table" "hide table" <$> showTableD)

  return never



