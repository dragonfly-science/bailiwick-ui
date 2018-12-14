{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RecursiveDo         #-}
module Bailiwick.View.ToolBar (
    toolBar
) where

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
import Reflex (TriggerEvent, demuxed, demux, foldDyn)
import Reflex.Dom.Core
       (elDynClass', GhcjsDomSpace, elAttr', MonadHold, PostBuild,
        DomBuilder, Event, Dynamic, divClass, el, elClass, text,
        (=:), never, constDyn, elDynAttr, holdDyn, leftmost,
        tag, current, domEvent, EventName(Click, Focus, Blur), ffor,
        dynText, elDynClass, listViewWithKey, elDynAttr',
        DomBuilderSpace)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.FunctorMaybe (FunctorMaybe(..))
-- import Bailiwick.View.Header (dropdownMenu)

import Bailiwick.Store (Store)
import qualified Bailiwick.Store as Store
import Bailiwick.State
import Bailiwick.Types

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
    => Dynamic t Store -> Dynamic t State -> m (Event t Message, Dynamic t Bool)
toolBar storeD stateD = do
  let areaTypes = OM.fromList [("nz", "New Zealand"), ("reg", "Regional Council"), ("ta", "Territorial Authority")]
      transforms = (\n -> OM.fromList [("indexed", "indexed"), ("absolute", fromMaybe "absolute" n)]) <$> absoluteLabel
      absoluteLabel = do
         mtp <- getThemePage <$> stateD
         indicators <- Store.getIndicators <$> storeD
         return $ do
             themepage <- mtp
             ind <- themePageIndicatorId themepage `OM.lookup` indicators
             indicatorAbsoluteLabel ind
      years = OM.fromList [(T.pack $ show y, T.pack $ show y)
                          | y <- reverse ([1996..2017] :: [ Int ])] -- TODO fix range
      areaTypeD = fmap themePageAreaType . getThemePage <$> stateD
      leftTransformD = fmap themePageLeftTransform . getThemePage <$> stateD
      rightChartD = fmap themePageRightChart . getThemePage <$> stateD
      yearD = fmap (T.pack . show . themePageYear) . getThemePage <$> stateD
      setAreaEvent = fmap (fmap SetAreaType . fmapMaybe id)
      setLeftTransformEvent = fmap (fmap SetLeftTransform . fmapMaybe id)
      setYearEvent = fmap (fmap SetYear . fmapMaybe id . fmap (readMaybe . T.unpack =<<))
  divClass "tool-bar" $ do
    (dropdownsE, isOpen) <- divClass "summary content" $ do
      dropdownsE <- divClass "top" $ do
        elClass "span" "label" $ text "select:"
        divClass "elements" $ do
          areaTypeE <- setAreaEvent $ divClass "element" $
            el "div" $
              toolbarDropdown "area" (constDyn "") never (constDyn True) areaTypeD (constDyn areaTypes)
          transformE <- setLeftTransformEvent $ divClass "element" $
            divClass "toolbar-transform" $
              toolbarDropdown "transform" (constDyn "") never (constDyn True) leftTransformD
                transforms
          yearE <- setYearEvent $ divClass "element" $
            el "div" $
              toolbarDropdown "year" (constDyn "") never (constDyn True) yearD
                (constDyn years)
          return $ leftmost [areaTypeE, transformE, yearE]
      isOpen <- divClass "actions content" $ mdo
        (b, _) <- elDynAttr' "button" (("class" =:) . bool "open-button" "close-button" <$> isOpen) $
            el "i" $ return ()
        isOpen <- foldDyn (const not) False $ domEvent Click b
        return isOpen
      return (dropdownsE, isOpen)
    filterE <- divClass "filtering content" $
      divClass "filters" $ do
        divClass "filter-type view-by" $
          elClass "span" "label" $ text "view by:"
        areaTypeE <- setAreaEvent $ toolbarList "area" (constDyn "") never (constDyn True) areaTypeD
          (constDyn areaTypes)
        transformE <- setLeftTransformEvent $ toolbarList "transform" (constDyn "") never (constDyn True) leftTransformD
          transforms
        yearE <- setYearEvent $ toolbarList "year" (constDyn "") never (constDyn True) yearD
          (constDyn years)
        rightTransformE <- divClass "filter-type charts" $ do
          elClass "span" "label" $ text "view by"
          divClass "header" $ do
            (ts, _) <- elDynClass' "button" (("timeseries" <>) . bool "" " active" . (==Just (ChartId "timeseries")) <$> rightChartD) $ el "i" $ return ()
            (bc, _) <- elDynClass' "button" (("barchart" <>) . bool "" " active" . (==Just (ChartId "barchart")) <$> rightChartD) $ el "i" $ return ()
            return $ leftmost
                [ SetRightChart (ChartId "timeseries") <$ domEvent Click ts
                , SetRightChart (ChartId "barchart") <$ domEvent Click bc
                ]
        return $ leftmost [areaTypeE, transformE, yearE, rightTransformE]
    return (leftmost [dropdownsE, filterE], isOpen)

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
          <- elAttr' "div" ("style" =: "overflow: scroll") $ -- ("class" =: "ps-content ps-container ps-theme-default") $
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
