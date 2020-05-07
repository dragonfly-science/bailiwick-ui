{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RecordWildCards     #-}
module Bailiwick.View.ToolBar (
    toolBar
  , ToolBarState(..)
) where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))
import Data.Bool (bool)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.Map (Map)
import qualified Data.Map as M (fromList, toList)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OM (toList, fromList, lookup)
import Text.Read (readMaybe)

import Language.Javascript.JSaddle (MonadJSM)
import Reflex (TriggerEvent, demuxed, demux)
import Reflex.Dom.Core
       (elDynClass', GhcjsDomSpace, elAttr', MonadHold, PostBuild,
        DomBuilder, Event, Dynamic, divClass, el, elClass, text,
        (=:), never, constDyn, elDynAttr, holdDyn, leftmost,
        tag, current, domEvent, EventName(Click, Focus, Blur), ffor,
        dynText, elDynClass, listViewWithKey, elDynAttr',
        DomBuilderSpace, constant)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.FunctorMaybe (FunctorMaybe(..))

import Bailiwick.Route
import Bailiwick.Types

data ToolBarState t
  = ToolBarState
  { indicatorD  :: Dynamic t (Maybe Indicator)
  , areaTypeD   :: Dynamic t (Maybe AreaType)
  , transformD  :: Dynamic t (Maybe TransformId)
  , chartTypeD  :: Dynamic t (Maybe ChartId)
  , yearD       :: Dynamic t (Maybe Year)
  }

toolBar
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadJSM (Performable m)
       , DomBuilderSpace m ~ GhcjsDomSpace
       )
    => Dynamic t Bool
    -> ToolBarState t
    -> m (Event t (Either () Message))
toolBar isOpenD ToolBarState{..} = do
  let areaTypes = OM.fromList [ ("nz", "New Zealand")
                              , ("reg", "Regional Council")
                              , ("ta", "Territorial Authority")
                              , ("ward", "Auckland wards")]
      absoluteLabel = do
        ind <- indicatorD
        return (join $ fmap indicatorAbsoluteLabel ind)
      transforms = (\n -> OM.fromList [ ("indexed", "indexed")
                                      , ("absolute", fromMaybe "absolute" n)]
                   ) <$> absoluteLabel
      yearsD = do
        ind <- indicatorD
        return $ OM.fromList [(T.pack $ show y, T.pack $ show y)
                             | y <- reverse (maybe ([]) indicatorYears ind)]

      textYearD = fmap (T.pack . show) <$> yearD

      chartsD = do
        ind <- indicatorD
        return $ M.fromList $ filter (\(k, _) -> unChartId k /= "map") $
                 maybe [] OM.toList (ind >>= indicatorCharts)

      chartButtonClass "over-under-barchart" = "barchart"
      chartButtonClass c = c

      setAreaEvent           = fmap (fmap SetAreaType . fmapMaybe id)
      setTransformEvent      = fmap (fmap SetTransform . fmapMaybe id)
      setYearEvent           = fmap (fmap SetYear . fmapMaybe id . fmap (readMaybe . T.unpack =<<))
      setChartTypeEvent      = fmap (fmap SetChartType)

  divClass "tool-bar" $ do
    dropdownsE <- divClass "summary content" $ do
      dropdownsE <- divClass "top" $ do
        elClass "span" "label" $ text "select:"
        divClass "elements" $ do
          areaTypeE <- setAreaEvent $ divClass "element" $
            el "div" $
              toolbarDropdown "area" (constDyn "") never (constDyn True) areaTypeD (constDyn areaTypes)
          transformE <- setTransformEvent $ divClass "element" $
            divClass "toolbar-transform" $
              toolbarDropdown "transform" (constDyn "") never (constDyn True) transformD
                transforms
          yearE <- setYearEvent $ divClass "element" $
            el "div" $
              toolbarDropdown "year" (constDyn "") never (constDyn True) textYearD yearsD
          return $ leftmost [areaTypeE, transformE, yearE]
      isOpenE <- divClass "actions content" $ mdo
        (b, _) <- elDynAttr' "button" (("class" =:) . bool "open-button" "close-button" <$> isOpenD) $
            el "i" $ return ()
        return $ () <$ domEvent Click b
      return $ leftmost [Left <$> isOpenE, Right <$> dropdownsE]
    filterE <- divClass "filtering content" $
      divClass "filters" $ do
        divClass "filter-type view-by" $
          elClass "span" "label" $ text "view by:"
        areaTypeE <- setAreaEvent $ toolbarList "area" (constDyn "") never (constDyn True) areaTypeD
          (constDyn areaTypes)
        transformE <- setTransformEvent $ toolbarList "transform" (constDyn "") never (constDyn True) transformD
          transforms
        yearE <- setYearEvent $ toolbarList "year" (constDyn "") never (constDyn True) textYearD yearsD
        chartTypeE <- setChartTypeEvent $ divClass "filter-type charts" $ do
          elClass "span" "label" $ text "view by"
          selectedChart :: Event t (Map ChartId ChartId)
            <- divClass "header" $ do
               let selectionDemux = demux chartTypeD
               listViewWithKey chartsD $ \k _ -> do
                 let selected = demuxed selectionDemux (Just k)
                 (b, _) <- elDynClass' "button" (((chartButtonClass (unChartId k)) <>) . bool "" " active" <$> selected) $ el "i" $ return ()
                 return (tag (constant k) (domEvent Click b))
          return $ fmap (head . (fmap fst) . M.toList) selectedChart
        return $ leftmost [areaTypeE, yearE, transformE, chartTypeE]
    return $ leftmost [dropdownsE, Right <$> filterE]

toolbarDropdown
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Text
    -> Dynamic t Text              -- empty value presentation
    -> Event t ()                  -- Close event
    -> Dynamic t Bool              -- Is hidden or not
    -> Dynamic t (Maybe Text)      -- Initial value
    -> Dynamic t (InsOrdHashMap Text Text)    -- Options (ordered)
    -> m (Event t (Maybe Text))
toolbarDropdown dropdownClass emptyPresentD closeE seenD currentValue valuesD = do

  let dropdownAttrD = do
        canSee <- seenD
        let visibility = if canSee then "visibility: block"
                                   else "visibility: hidden"
        return ("class" =: ("toolbar-dropdown " <> dropdownClass) <> "style" =: visibility)

  elDynAttr "div" dropdownAttrD $ mdo

      active :: Dynamic t Bool
        <- holdDyn False $
            leftmost [ True <$ domEvent Focus container
                     , False <$ domEvent Blur container ]

      open :: Dynamic t Bool
        <- holdDyn False $
            leftmost [ not <$> tag (current open) (domEvent Click a)
                     , False <$ selectedValue
                     , False <$ closeE ]

      let label = do
            mval <- currentValue
            case mval of
              Nothing -> emptyPresentD
              Just val -> fromMaybe "" . OM.lookup val <$> valuesD

          containerClass = (\isOpen isActive ->
              "chosen-container chosen-container-single chosen-container-single-nosearch"
              <> (if isOpen then " chosen-with-drop"
                            else "")
              <> (if isActive then " chosen-container-active"
                              else "")) <$> open <*> active

          ulClass = ffor open $ \isOpen ->
              if isOpen then "chosen-results show-menu"
                        else "chosen-results"

          shuffle i (k, v) = ((i,k), v)
          optionsD = M.fromList . zipWith shuffle [1 ..] . OM.toList <$> valuesD

      (container, (a, selectedValue)) <-
        elDynAttr' "div" (("tabindex" =: "0" <>) . ("class" =:) <$> containerClass) $ do
          (a', _) <-  elDynAttr' "a" (constDyn $ "class" =: "chosen-single") $ do
            el "span" $ dynText label
            el "div" $ el "b" $ return ()
          selectedValue' :: Event t (Map (Int, Text) Text)
            <- divClass "chosen-drop" . elDynClass "ul" ulClass $ do
                let selectionDemux = demux currentValue
                listViewWithKey optionsD $ \(_, k) v -> do
                  let selected = demuxed selectionDemux (Just k)
                  (li, _) <- elDynAttr' "li" (("class" =:) . ("active-result" <>) . bool "" " result-selected" <$> selected) $ dynText v
                  return (tag (current v) (domEvent Click li))
          return (a', selectedValue')

      return $ firstKey <$> selectedValue

  where
    firstKey :: Map (Int, Text) Text -> Maybe Text
    firstKey = fmap (snd . fst) . listToMaybe . M.toList

toolbarList
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadJSM (Performable m)
       , DomBuilderSpace m ~ GhcjsDomSpace
       )
    => Text
    -> Dynamic t Text              -- empty value presentation
    -> Event t ()                  -- Close event
    -> Dynamic t Bool              -- Is hidden or not
    -> Dynamic t (Maybe Text)      -- Initial value
    -> Dynamic t (InsOrdHashMap Text Text)    -- Options (ordered)
    -> m (Event t (Maybe Text))
toolbarList dropdownClass emptyPresentD closeE seenD currentValue valuesD =
  divClass ("filter-type " <> dropdownClass) $ mdo

      let shuffle i (k, v) = ((i,k), v)
          optionsD = M.fromList . zipWith shuffle [1 ..] . OM.toList <$> valuesD

      elClass "span" "label" $ text dropdownClass
      (_, selectedValue :: Event t (Map (Int, Text) Text))
          <- elAttr' "div" ("style" =: "") $ -- ("class" =: "ps-content ps-container ps-theme-default") $
                elClass "ul" "options" $ do
                let selectionDemux = demux currentValue
                listViewWithKey optionsD $ \(_, k) v -> do
                  let selected = demuxed selectionDemux (Just k)
                  (li, _) <- el "li" $ elDynAttr' "button" (("class" =:) . bool "" "active" <$> selected) $
                    dynText v
                  return (tag (current v) (domEvent Click li))
--      postBuild <- delay 20 =<< getPostBuild
--      performEvent_ $ ffor postBuild $ \() -> do
--            liftJSM $ new (jsg ("PerfectScrollbar" :: Text)) [_element_raw ps :: Element]
--            return ()
      tbE <- toolbarDropdown dropdownClass emptyPresentD closeE seenD currentValue valuesD
      return $ leftmost [tbE, firstKey <$> selectedValue]

  where
    firstKey :: Map (Int, Text) Text -> Maybe Text
    firstKey = fmap (snd . fst) . listToMaybe . M.toList
