{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Bailiwick.View.IndicatorTable
  ( indicatorTable
  , IndicatorTableState(..)
  )
where

import Data.Bool (bool)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict.InsOrd as OMap

import Reflex.Dom.Core

import Bailiwick.Route
import Bailiwick.Types

data IndicatorTableState t
  = IndicatorTableState
  { showTableD         :: Dynamic t Bool
  , routeD             :: Dynamic t Route
  , areaD              :: Dynamic t (Maybe Area)
  , compareAreaD       :: Dynamic t (Maybe Area)
  , featureD           :: Dynamic t (Maybe FeatureId)
  , indicatorD         :: Dynamic t (Maybe Indicator)
  , indicatorNumbersD  :: Dynamic t IndicatorNumbers
  }


type IndicatorTable = OMap.InsOrdHashMap (AreaId, Year, Maybe FeatureId) (Year, Numbers)

shapeData :: Maybe Area -> Maybe FeatureId -> IndicatorNumbers -> IndicatorTable
shapeData marea sel_featureid (IndicatorNumbers inmap) =
  let sel_areaid = maybe "none" areaId marea
      find (areaid, year, featureid) numbers =
        if (sel_areaid == areaid && sel_featureid == featureid)
            then Just (year, numbers)
            else Nothing
  in  OMap.mapMaybeWithKey find inmap

indicatorTable
  :: ( Monad m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     )
  => IndicatorTableState t
  -> m (Event t Message)
indicatorTable IndicatorTableState{..} = do

  let tableD = shapeData <$> areaD <*> featureD <*> indicatorNumbersD

  clickCloseE <- do
    elDynClass "div" (("table-view " <>) . bool "hide" "show" <$> showTableD) $
      elAttr "div" ("class" =: "panel" <> "style" =: "height: 799px;") $ do
        clickE <-
          el "header" $ do
            divClass "table-caption text" $
              text "TODO"
            fmap (domEvent Click . fst) $
              divClass "controls" $ do
                -- requires an event to close the table.
                elAttr' "button" ("class" =: "close") $
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
              el "tbody" $ do
                dyn_ $ ffor tableD $ mapM $ \(year, Numbers{..}) -> do
                  el "tr" $ do
                    el "td" $ text (Text.pack $ show year)
                    elAttr "td" ("class" =: "colour-teal") $ text headlineDisp
                    elAttr "td" ("class" =: "colour-lighter-blue") $ text localDisp
                    elAttr "td" ("class" =: "colour-green") $ text nationalDisp

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
        return clickE

  showTableE <- fmap (domEvent Click . fst) $
    divClass "table-button" $
      elClass' "button" "show-table" $
        el "span" $ do
          el "i" $ return ()
          dynText (bool "show table" "hide table" <$> showTableD)

  let openclose = bool SetShowTable UnsetShowTable
  return (tagPromptlyDyn (openclose <$> showTableD) (leftmost [showTableE, clickCloseE]))
