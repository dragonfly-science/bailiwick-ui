{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.IndicatorTable
  ( indicatorTable
  , IndicatorTableState(..)
  )
where

import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sortOn)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict.InsOrd as OMap

import Reflex.Dom.Core

import Bailiwick.View.Text
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

data Column = YearCol | OriginalCol | TransformCol | NationalCol deriving Eq
instance Show Column where
  show YearCol = "year"
  show OriginalCol = "original"
  show TransformCol = "transform"
  show NationalCol = "national"
data Order = Asc | Desc deriving (Eq, Show)
type SortOrder = (Column, Order)
type IndicatorTable = [(Year, Numbers)]

shapeData
  :: Maybe Area
  -> Maybe FeatureId
  -> SortOrder
  -> IndicatorNumbers
  -> IndicatorTable
shapeData marea sel_featureid (sortcol,sortdir) (IndicatorNumbers inmap) =
  let sel_areaid = maybe "none" areaId marea
      find (areaid, year, featureid) numbers =
        if (sel_areaid == areaid && sel_featureid == featureid)
            then Just (year, numbers)
            else Nothing
      sfc = case sortcol of
             YearCol -> Just . fromIntegral . fst
             OriginalCol  -> rawNum . snd
             TransformCol -> localNum . snd
             NationalCol  -> nationalNum . snd
      sf = case sortdir of
             Asc -> sfc
             Desc -> fmap negate . sfc
  in  sortOn sf $ OMap.elems $ OMap.mapMaybeWithKey find inmap

indicatorTable
  :: ( Monad m
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     )
  => IndicatorTableState t
  -> m (Event t Message)
indicatorTable IndicatorTableState{..} = mdo

  sortOrderD :: Dynamic t SortOrder
    <- holdDyn (YearCol, Desc) sortOrderE

  let tableD = shapeData <$> areaD <*> featureD <*> sortOrderD <*> indicatorNumbersD

      pageD = getThemePage <$> routeD
      subs = (textSubstitution
                    <$> areaD
                    <*> compareAreaD
                    <*> indicatorD
                    <*> featureD
                    <*> pageD
                    <*>)

      captionsD = fmap indicatorCaptions <$> indicatorD
      transform = "annual-rate"
      headlineD = (OMap.lookup "original" =<<) <$> captionsD
      localD    = (OMap.lookup transform  =<<) <$> captionsD
      nationalD = (OMap.lookup "ratio-nz" =<<) <$> captionsD

      yearEndMonthD = (indicatorYearEndMonth =<<) <$> indicatorD

      captionD = do
        headline     <- headlineD
        local        <- localD
        national     <- nationalD
        yearEndMonth <- yearEndMonthD
        let cols = Text.intercalate "; " $ catMaybes [headline, local, national]
        let suffix = maybe "" (\m -> " Data are for the year to " <> m <> ".") yearEndMonth
        return $ "The table shows " <> cols <> "." <> suffix

      labelsD = fmap indicatorLabels <$> indicatorD
      headlineLabelD = (OMap.lookup "original" =<<) <$> labelsD
      localLabelD    = (OMap.lookup transform  =<<) <$> labelsD
      nationalLabelD = (OMap.lookup "ratio-nz" =<<) <$> labelsD

  (clickCloseE, sortOrderE) <- do
    elDynClass "div" (("table-view " <>) . bool "hide" "show" <$> showTableD) $
      elAttr "div" ("class" =: "panel" <> "style" =: "height: 799px;") $ do
        clickE <-
          el "header" $ do
            divClass "table-caption text" $
              dynText $ subs $ captionD
            fmap (domEvent Click . fst) $
              divClass "controls" $ do
                -- requires an event to close the table.
                elAttr' "button" ("class" =: "close") $
                  el "i" $ return ()

        sortOrderE' <-
          elAttr "div" ("class" =: "table-container" <> "style" =: "height: 657px;") $

            divClass "single-table" $ do
              -- may need logic to determine when to use "show" on the table class.
              elAttr "table" ("class" =: "table-sorter show") $ do
                el "tfoot" $
                  el "tr" $
                    elAttr "td" ("class" =: "button" <> "colspan" =: "4") $ do
                      -- there needs to be logic to check whether there can be
                      -- an export button
                      elAttr "button" ("class" =: "export") $ text "Export CSV"
                sortOrderE'' <-
                  el "thead" $
                    el "tr" $ do
                      -- Each th element needs an event to add "tablesorter-headerDesc" or
                      -- "tablesorter-headerAsc" if the column is being sorted.
                      let tableSortAttrD col = do
                            (sortCol, direction) <- sortOrderD
                            let tablesortcss =
                                    if sortCol == col
                                        then " tablesorter-header" <> (Text.pack $ show direction)
                                        else ""
                            return ("class" =: ((Text.toLower $ Text.pack $ show col) <> tablesortcss))

                      yearClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD YearCol) $
                          text "Year"
                      originalClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD OriginalCol) $
                          dynText $ subs (fromMaybe "" <$> headlineLabelD)
                      transformClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD TransformCol) $
                          dynText $ subs (fromMaybe "" <$> localLabelD)
                      nationalClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD NationalCol) $
                          dynText $ subs (fromMaybe "" <$> nationalLabelD)

                      let f (_, Desc) col = (col, Asc)
                          f (_, Asc) col = (col, Desc)
                      return $ attachWith f (current sortOrderD) $
                                 leftmost [ YearCol      <$ yearClickE
                                          , OriginalCol  <$ originalClickE
                                          , TransformCol <$ transformClickE
                                          , NationalCol  <$ nationalClickE
                                          ]
                el "tbody" $ do
                  dyn_ $ ffor tableD $ mapM $ \(year, Numbers{..}) -> do
                    el "tr" $ do
                      el "td" $ text (Text.pack $ show year)
                      elAttr "td" ("class" =: "colour-teal") $ text headlineDisp
                      elAttr "td" ("class" =: "colour-lighter-blue") $ text localDisp
                      elAttr "td" ("class" =: "colour-green") $ text nationalDisp

                return sortOrderE''

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
        return (clickE, sortOrderE')

  showTableE <- fmap (domEvent Click . fst) $
    divClass "table-button" $
      elClass' "button" "show-table" $
        el "span" $ do
          el "i" $ return ()
          dynText (bool "show table" "hide table" <$> showTableD)

  let openclose = bool SetShowTable UnsetShowTable
  return (tagPromptlyDyn (openclose <$> showTableD) (leftmost [showTableE, clickCloseE]))
