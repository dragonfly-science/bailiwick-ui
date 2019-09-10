{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.IndicatorTable
  ( indicatorTable
  , IndicatorTableState(..)
  )
where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sortOn)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Csv as Csv

import Reflex.Dom.Core hiding (elDynHtmlAttr')
import GHCJS.DOM.Types (BlobPropertyBag(..))
import GHCJS.DOM.URL (createObjectURL)
import GHCJS.DOM.Blob (newBlob)
import Foreign.JavaScript.Utils (bsToArrayBuffer)
import Language.Javascript.JSaddle (obj, (<#), MonadJSM, liftJSM, toJSVal)

import Bailiwick.View.Text
import Bailiwick.Javascript (switchDynM, elDynHtmlAttr')
import Bailiwick.Route
import Bailiwick.Types

data IndicatorTableState t
  = IndicatorTableState
  { showTableD         :: Dynamic t Bool
  , areaD              :: Dynamic t (Maybe Area)
  , compareAreaD       :: Dynamic t (Maybe Area)
  , featureD           :: Dynamic t (Maybe FeatureId)
  , yearD              :: Dynamic t (Maybe Year)
  , indicatorD         :: Dynamic t (Maybe Indicator)
  , indicatorNumbersD  :: Dynamic t IndicatorNumbers
  }

data Column
  = YearCol
  | OriginalCol
  | TransformCol
  | NationalCol
  | OriginalAreaCol
  | OriginalCompCol
  | RatioCol
  | NationalAreaCol
  | NationalCompCol
  deriving Eq
instance Show Column where
  show YearCol             = "year"
  show OriginalCol         = "original"
  show TransformCol        = "transform"
  show NationalCol         = "national"
  show OriginalAreaCol     = "original-area"
  show OriginalCompCol     = "original-compare"
  show RatioCol            = "ratio"
  show NationalAreaCol     = "national-area"
  show NationalCompCol     = "national-compare"
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
             YearCol      -> Just . fromIntegral . fst
             OriginalCol  -> rawNum . snd
             TransformCol -> localNum . snd
             NationalCol  -> nationalNum . snd
             _            -> Just . fromIntegral . fst
      sf = case sortdir of
             Asc -> sfc
             Desc -> fmap negate . sfc
  in  sortOn sf $ OMap.elems $ OMap.mapMaybeWithKey find inmap

type CompareTable = [(Year, ((Numbers, Numbers), Maybe Double))]
shapeCompareData
  :: Maybe Area
  -> Maybe Area
  -> Maybe FeatureId
  -> SortOrder
  -> IndicatorNumbers
  -> CompareTable
shapeCompareData marea mcomp sel_featureid (sortcol,sortdir) (IndicatorNumbers inmap) =
  let sel_areaid = maybe "none" areaId marea
      sel_compid = maybe "none" areaId mcomp
      find aid (areaid, year, featureid) numbers =
        if (aid == areaid && sel_featureid == featureid)
            then Just (year, numbers)
            else Nothing
      areaData = OMap.fromList $ OMap.elems $ OMap.mapMaybeWithKey (find sel_areaid) inmap
      compData = OMap.fromList $ OMap.elems $ OMap.mapMaybeWithKey (find sel_compid) inmap
      compareTable = [(y, ((ad, cd)
                          , (/) <$> (rawNum ad) <*> (rawNum cd)))
                     | y <- OMap.keys (OMap.union areaData compData)
                     , let ad = fromMaybe emptyNumbers $ OMap.lookup y areaData
                     , let cd = fromMaybe emptyNumbers $ OMap.lookup y compData
                     ]
      sfc = case sortcol of
             YearCol          -> Just . fromIntegral . fst
             OriginalAreaCol  -> rawNum . fst . fst . snd
             OriginalCompCol  -> rawNum . snd . fst . snd
             NationalAreaCol  -> nationalNum . fst . fst . snd
             NationalCompCol  -> nationalNum . snd . fst . snd
             _                -> Just . fromIntegral . fst
      sf = case sortdir of
             Asc -> sfc
             Desc -> fmap negate . sfc
  in  sortOn sf compareTable


exportCSVLink
  :: ( Monad m
     , DomBuilder t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Dynamic t ByteString
  -> m (Event t Text)
exportCSVLink csvD = do

  performEvent $ ffor (updated csvD) $ \csv -> do
    liftJSM $ do
        arrayBuffer <- bsToArrayBuffer $ toStrict csv
        o <- obj
        (o <# ("type" :: Text)) ("text/csv;charset=utf-8"::Text)
        props <- BlobPropertyBag <$> toJSVal o
        blob <- newBlob [arrayBuffer] (Just props)
        createObjectURL blob

indicatorTable
  :: ( Monad m
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => IndicatorTableState t
  -> m (Event t Message)
indicatorTable its@IndicatorTableState{..} = do

  clickCloseE <- switchDynM $ ffor (zipDyn showTableD compareAreaD) $ \case
    (True, Nothing)  -> tableView its
    (True, Just _)   -> compareTableView its
    (False, _)       -> return never

  showTableE <- fmap (domEvent Click . fst) $
    divClass "table-button" $
      elClass' "button" "show-table" $
        el "span" $ do
          el "i" $ return ()
          dynText (bool "show table" "hide table" <$> showTableD)

  let openclose = bool SetShowTable UnsetShowTable

  return (tag (openclose <$> (current showTableD))
              (leftmost [showTableE, clickCloseE]))


tableView
  :: ( Monad m
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadJSM (Performable m)
     )
  => IndicatorTableState t
  -> m (Event t ())
tableView IndicatorTableState{..} = mdo

  sortOrderD :: Dynamic t SortOrder
    <- holdDyn (YearCol, Desc) sortOrderE

  let tableD = shapeData <$> areaD <*> featureD <*> sortOrderD <*> indicatorNumbersD

      subs = (textSubstitution
                    <$> areaD
                    <*> compareAreaD
                    <*> indicatorD
                    <*> featureD
                    <*> (constDyn Nothing)
                    <*> yearD
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

  (clickCloseE, sortOrderE) <-
    elDynClass "div" (("table-view " <>) . bool "hide" "show" <$> showTableD) $
      elAttr "div" ("class" =: "panel" <> "style" =: "height: 799px;") $ do
        clickE <-
          el "header" $ do
            divClass "table-caption text" $
              dynText $ subs $ captionD
            fmap (domEvent Click . fst) $
              divClass "controls" $ do
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
                      let csvD = do
                            headline <- subs (fromMaybe "Headline" <$> headlineLabelD)
                            local    <- subs (fromMaybe "Local" <$> localLabelD)
                            national <- subs (fromMaybe "National" <$> nationalLabelD)
                            source   <- subs (maybe "" indicatorPublishers <$> indicatorD)
                            let header = ( "Year"::Text
                                         , headline
                                         , local
                                         , national
                                         , "Copyright"::Text
                                         , "Owner"::Text
                                         )
                            table <- tableD
                            let contents = [ ( Text.pack $ show year
                                             , maybe "" (Text.pack . show) rawNum
                                             , maybe "" (Text.pack . show) localNum
                                             , maybe "" (Text.pack . show) nationalNum
                                             , "CC-BY-4" :: Text
                                             , source
                                             )
                                           | (year, Numbers{..}) <- table ]
                            return $ Csv.encode (header:contents)

                      urlE <- exportCSVLink csvD
                      urlD <- holdDyn "" urlE
                      let exportAttrD = do
                            filename <- subs (constDyn "$indid$-$yearEndMonth$-$areaid$.csv")
                            url <- urlD
                            return (  "class"    =: "export"
                                   <> "href"     =: url
                                   <> "download" =: filename )
                      elDynAttr "a" exportAttrD $ text "Export CSV"

                sortOrderE'' <-
                  el "thead" $
                    el "tr" $ do
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
        return (clickE, sortOrderE')
  return clickCloseE

compareTableView
  :: ( Monad m
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => IndicatorTableState t
  -> m (Event t ())
compareTableView IndicatorTableState{..} = mdo

  sortOrderD :: Dynamic t SortOrder
    <- holdDyn (YearCol, Desc) sortOrderE

  let tableD = shapeCompareData <$> areaD <*> compareAreaD <*> featureD <*> sortOrderD <*> indicatorNumbersD

      subs = (textSubstitution
                    <$> areaD
                    <*> compareAreaD
                    <*> indicatorD
                    <*> featureD
                    <*> (constDyn Nothing)
                    <*> yearD
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

  (clickCloseE, sortOrderE) <-
    elDynClass "div" (("table-view " <>) . bool "hide" "show" <$> showTableD) $
      elAttr "div" ("class" =: "panel" <> "style" =: "height: 799px;") $ do
        clickE <-
          el "header" $ do
            divClass "table-caption text" $
              dynText $ subs $ captionD
            fmap (domEvent Click . fst) $
              divClass "controls" $ do
                elAttr' "button" ("class" =: "close") $
                  el "i" $ return ()

        sortOrderE' <-
          elAttr "div" ("class" =: "table-container" <> "style" =: "height: 657px;") $
            divClass "compare-table" $ do
              elAttr "table" ("class" =: "table-sorter show") $ do
                el "tfoot" $
                  el "tr" $ do
                    elAttr "td" ("class" =: "button") $ do
                      let csvD = do
                            headline <- subs (fromMaybe "Headline" <$> headlineLabelD)
                            local    <- subs (fromMaybe "Local" <$> localLabelD)
                            national <- subs (fromMaybe "National" <$> nationalLabelD)
                            source   <- subs (maybe "" indicatorPublishers <$> indicatorD)
                            let header = ( "Year"::Text
                                         , headline
                                         , local
                                         , national
                                         , "Copyright"::Text
                                         , "Owner"::Text
                                         )
                            table <- tableD
                            let contents = [ ( Text.pack $ show year
                                             , maybe "" (Text.pack . show) rawNum
                                             , maybe "" (Text.pack . show) localNum
                                             , maybe "" (Text.pack . show) nationalNum
                                             , "CC-BY-4" :: Text
                                             , source
                                             )
                                           | (year, ((Numbers{..}, _),_)) <- table ]
                            return $ Csv.encode (header:contents)

                      urlE <- exportCSVLink csvD
                      urlD <- holdDyn "" urlE
                      let exportAttrD = do
                            filename <- subs (constDyn "$indid$-$yearEndMonth$-$areaid$.csv")
                            url <- urlD
                            return (  "class"    =: "export"
                                   <> "href"     =: url
                                   <> "download" =: filename )
                      elDynAttr "a" exportAttrD $ text "Export CSV"
                    elAttr "td" ("colspan" =: "3") $ do
                      void $ elDynHtmlAttr' "span" def $ subs ( fromMaybe "Headline" <$> headlineLabelD)
                    elAttr "td" ("colspan" =: "2") $ do
                      void $ elDynHtmlAttr' "span" def $ subs ( fromMaybe "National" <$> nationalLabelD)

                sortOrderE'' <-
                  el "thead" $
                    el "tr" $ do
                      let tableSortAttrD base col = do
                            (sortCol, direction) <- sortOrderD
                            let tablesortcss =
                                    if sortCol == col
                                        then " " <> base <> " tablesorter-header" <> (Text.pack $ show direction)
                                        else " " <> base
                            return ("class" =: ((Text.toLower $ Text.pack $ show col) <> tablesortcss))

                      yearClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD "double" YearCol) $
                          text "Year"
                      originalAreaClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD "border-top" OriginalAreaCol) $
                          dynText $ subs (maybe "" areaName <$> areaD)
                      originalCompareClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD "border-top" OriginalCompCol) $
                          dynText $ subs (maybe "" areaName <$> compareAreaD)
                      ratioClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD "border-top double" RatioCol) $
                          text "Ratio"
                      nationalAreaClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD "border-top" NationalAreaCol) $
                          dynText $ subs (maybe "" areaName <$> areaD)
                      nationalCompareClickE <- fmap (domEvent Click . fst)  $
                        elDynAttr' "th" (tableSortAttrD "border-top" NationalCompCol) $
                          dynText $ subs (maybe "" areaName <$> compareAreaD)

                      let f (_, Desc) col = (col, Asc)
                          f (_, Asc) col = (col, Desc)
                      return $ attachWith f (current sortOrderD) $
                                 leftmost [ YearCol          <$ yearClickE
                                          , OriginalAreaCol  <$ originalAreaClickE
                                          , OriginalCompCol  <$ originalCompareClickE
                                          , RatioCol         <$ ratioClickE
                                          , NationalAreaCol  <$ nationalAreaClickE
                                          , NationalCompCol  <$ nationalCompareClickE
                                          ]
                el "tbody" $ do
                  dyn_ $ ffor tableD $ mapM $ \(year, ((area, comp), ratio)) -> do
                    let showRatio Nothing = "-"
                        showRatio (Just r) = show ((fromIntegral $ round (100.0 * r)) / 100.0)
                    el "tr" $ do
                      elAttr "td" ("class" =: "double") $  text (Text.pack $ show year)
                      elAttr "td" ("class" =: "colour-green") $ text (headlineDisp area)
                      elAttr "td" ("class" =: "colour-compare-green") $ text (headlineDisp comp)
                      elAttr "td" ("class" =: "double") $ text (Text.pack $ showRatio ratio)
                      elAttr "td" ("class" =: "colour-green") $ text (nationalDisp area)
                      elAttr "td" ("class" =: "colour-compare-green") $ text (nationalDisp comp)

                return sortOrderE''

        return (clickE, sortOrderE')
  return clickCloseE
